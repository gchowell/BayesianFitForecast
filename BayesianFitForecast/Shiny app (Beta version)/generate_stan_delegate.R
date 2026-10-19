#------------------------------------------------------------------------------#
# generate_stan_delegate.R — Generate the Stan program from an options file -----
#------------------------------------------------------------------------------#
# About: `generate_stan_file_delegated()` sources an options file into a clean  #
# environment and runs `generate_stan_file()` from stancreator.R inside that    #
# environment, so the Stan program is built from exactly the options the user   #
# selected (including time-dependent templates) without polluting the global   #
# environment of the Shiny session.                                            #
#                                                                              #
# The app folder now ships its own up-to-date copy of stancreator.R, so the    #
# app is self-contained. If the file is missing (e.g. it was deleted), the     #
# parent folder of the main toolbox is tried as a fallback.                    #
#------------------------------------------------------------------------------#

.bff_find_stancreator_dir <- function(explicit = NULL) {
  app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidates <- c(
    explicit,
    app_dir,
    normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE)
  )
  for (d in candidates) {
    if (!is.null(d) && file.exists(file.path(d, "stancreator.R"))) {
      return(normalizePath(d, winslash = "/", mustWork = FALSE))
    }
  }
  stop("Could not locate stancreator.R. Looked in: ",
       paste(candidates, collapse = ", "),
       ". Pass main_dir explicitly to generate_stan_file_delegated().")
}

# Kept for backwards compatibility with older code paths.
.tdp_find_main_dir <- .bff_find_stancreator_dir

generate_stan_file_delegated <- function(optionsFile, stan_file, main_dir = NULL) {

  main_dir <- .bff_find_stancreator_dir(main_dir)
  stancreator_path <- file.path(main_dir, "stancreator.R")

  env <- new.env(parent = globalenv())
  assign("stan_file", stan_file, envir = env)

  sys.source(normalizePath(optionsFile, winslash = "/", mustWork = TRUE), envir = env)

  if (!exists("time_dependent_templates", envir = env, inherits = FALSE)) {
    assign("time_dependent_templates", list(), envir = env)
  }
  if (!exists("composite_expressions", envir = env, inherits = FALSE)) {
    assign("composite_expressions", list(), envir = env)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(main_dir)

  sys.source(stancreator_path, envir = env)  # defines env$generate_stan_file()
  stan_code <- env$generate_stan_file()
  stan_code
}
