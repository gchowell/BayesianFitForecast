#------------------------------------------------------------------------------#
# Stan-generation delegation adapter                                            #
#------------------------------------------------------------------------------#
# About: The Shiny beta app must NOT reimplement time-dependent Stan generation #
# in its own (older) stancreator.R. Instead it builds a correct options file    #
# (including a `time_dependent_templates` block) and delegates Stan generation  #
# to the MAIN, updated stancreator.R that lives one directory up in the repo    #
# and already supports time_dependent_templates.                                #
#                                                                               #
# The main stancreator's generate_stan_file() takes no arguments: it reads the  #
# options as globals and writes to the global `stan_file`, and it source()s     #
# diff.R / ode_rhs.R relative to the working directory. This adapter therefore: #
#   1. loads the options file (incl. time_dependent_templates) into a private   #
#      environment,                                                             #
#   2. loads the MAIN stancreator into that same environment,                  #
#   3. temporarily switches the working directory to the main repo so the       #
#      main diff.R / ode_rhs.R are the ones used,                              #
#   4. calls the main generate_stan_file() and returns the Stan code.          #
#                                                                               #
# NOTE: the main stancreator emits modern Stan array syntax (array[] real ...), #
# which requires rstan / StanHeaders >= 2.26.                                   #
#------------------------------------------------------------------------------#

# Locate the main repo directory (the parent of the Shiny app folder). The app
# is sourced with the Shiny folder as the working directory, so ".." is the repo.
.tdp_find_main_dir <- function(explicit = NULL) {
  candidates <- c(
    explicit,
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
  )
  for (d in candidates) {
    if (!is.null(d) && file.exists(file.path(d, "stancreator.R"))) {
      return(normalizePath(d, winslash = "/", mustWork = FALSE))
    }
  }
  stop("Could not locate the main stancreator.R. Looked in: ",
       paste(candidates, collapse = ", "),
       ". Pass main_dir explicitly to generate_stan_file_delegated().")
}

# Delegate Stan generation to the main updated stancreator.
#   optionsFile : path to the options.R produced by the Shiny app
#   stan_file   : path where the .stan file should be written
#   main_dir    : (optional) explicit path to the main repo directory
# Returns the generated Stan code (character scalar).
generate_stan_file_delegated <- function(optionsFile, stan_file, main_dir = NULL) {

  main_dir <- .tdp_find_main_dir(main_dir)
  main_stancreator <- file.path(main_dir, "stancreator.R")

  # Private environment so we do not clobber the app's globals.
  env <- new.env(parent = globalenv())
  assign("stan_file", stan_file, envir = env)

  # 1. Load the options (defines params, vars, ode_system, paramsfix, priors,
  #    time_dependent_templates, errstrc, fitting_index, fitting_diff, ...).
  sys.source(normalizePath(optionsFile, winslash = "/", mustWork = TRUE),
             envir = env)

  # Back-compat: options files produced before this feature omit the templates.
  if (!exists("time_dependent_templates", envir = env, inherits = FALSE)) {
    assign("time_dependent_templates", list(), envir = env)
  }

  # 2/3. Run the main stancreator with the main repo as working directory so its
  #      relative source("diff.R") / source("ode_rhs.R") resolve to the main copies.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(main_dir)

  sys.source(main_stancreator, envir = env)  # defines env$generate_stan_file()

  # 4. Generate. The main generate_stan_file() reads globals from `env` and also
  #    writes the file to env$stan_file; we return the code as well.
  stan_code <- env$generate_stan_file()
  stan_code
}
