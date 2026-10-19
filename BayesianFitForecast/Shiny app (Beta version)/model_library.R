#------------------------------------------------------------------------------#
# model_library.R — Library of ready-to-use models ------------------------------
#------------------------------------------------------------------------------#
# About: Seven classical models (EXP, GLM, Richards, Gompertz, SIR, SEIR, SEIRD) #
# with their state variables, initial conditions, parameters (default priors or #
# fixed values), fitted quantity, composite quantities, equations, assumptions  #
# and references. Choosing a model in the app fills every step of the form      #
# (state variables, parameters & priors, equations, composites); a "model card" #
# documents each model in the app and in the report saved with every run.      #
#                                                                              #
# Equations are written with internal names (S, I, beta, N, ...) and are turned  #
# into keypad tokens / LaTeX with the helpers of ode_equation_builder_module.R, #
# so the library and the equation builder always render the same mathematics.  #
#------------------------------------------------------------------------------#

`%||%` <- get0("%||%", mode = "function", ifnotfound = function(x, y) if (is.null(x)) y else x)

#------------------------------------------------------------------------------#
# Constructors -----------------------------------------------------------------
#------------------------------------------------------------------------------#

# A state variable of a model
#   name     internal name shown in the form (letters/digits)
#   meaning  short description for the model card
#   ic       initial condition as typed in the math field (LaTeX; e.g. "N-i_0")
#   fit      TRUE when this compartment is fitted to a data column
#   series   index of the data column fitted (cases1 = 1, cases2 = 2, ...)
#   diff     TRUE when the derivative (new cases per time unit) is fitted
bff_model_state <- function(name, meaning, ic, fit = FALSE, series = 1L, diff = TRUE) {
  list(name = name, meaning = meaning, ic = ic, fit = isTRUE(fit),
       series = as.integer(series), diff = isTRUE(diff))
}

# A parameter of a model
#   latex      display name (LaTeX allowed, e.g. "\\beta")
#   name       internal identifier used in the equations / Stan ("beta")
#   meaning    description for the model card
#   fixed      TRUE = fixed value, FALSE = estimated with the prior below
#   value      fixed value (ignored when estimated)
#   prior      prior family (names of prior_catalog in app.R: "Normal", "Uniform", ...)
#   args       numeric arguments of the prior (e.g. mean and SD)
#   trunc      truncate the prior at zero (T[0,])
#   lb, ub     bounds declared in Stan ("NA" = none)
#   value_from "first_obs": the default value is the first observation of the fitted series
#   scale_from "total_cases": the prior is scaled with the total number of cases in the data
#   note       extra guidance shown in the model card
bff_model_param <- function(latex, name, meaning, fixed = FALSE, value = NA_real_,
                            prior = "Normal", args = c(0.5, 1), trunc = TRUE,
                            lb = "0", ub = "NA", value_from = NULL, scale_from = NULL, note = NULL) {
  list(latex = latex, name = name, meaning = meaning, fixed = isTRUE(fixed), value = value,
       prior = prior, args = args, trunc = isTRUE(trunc), lb = as.character(lb), ub = as.character(ub),
       value_from = value_from, scale_from = scale_from, note = note)
}

# A composite quantity (reported with a posterior distribution)
bff_model_composite <- function(name, latex, meaning) {
  list(name = name, latex = latex, meaning = meaning)
}

bff_model_def <- function(id, title, family, tagline, description, states, params, equations,
                          composites = list(), fitted_text, errstrc = "Negative binomial",
                          when_to_use = character(0), assumptions = character(0),
                          notes = character(0), references = character(0)) {
  list(id = id, title = title, family = family, tagline = tagline, description = description,
       states = states, params = params, equations = equations, composites = composites,
       fitted_text = fitted_text, errstrc = errstrc, when_to_use = when_to_use,
       assumptions = assumptions, notes = notes, references = references)
}

#------------------------------------------------------------------------------#
# References shared by the models ----------------------------------------------
#------------------------------------------------------------------------------#

bff_ref_toolbox <- paste(
  "Karami H, Bleichrodt A, Luo R, Chowell G (2025). BayesianFitForecast: A user-friendly R toolbox",
  "for parameter estimation and forecasting with ordinary differential equations.",
  "BMC Medical Informatics and Decision Making (arXiv:2411.05371).")
bff_ref_pii <- paste(
  "Karami H, Smirnova A, Lee S, Chowell G (2026). Parameter uncertainty in dynamical models:",
  "a practical identifiability index. arXiv:2606.08475.")
bff_ref_primer <- paste(
  "Chowell G (2017). Fitting dynamic models to epidemic outbreaks with quantified uncertainty:",
  "A primer for parameter uncertainty, identifiability, and forecasts. Infectious Disease Modelling 2(3): 379-398.")
bff_ref_ggm <- paste(
  "Viboud C, Simonsen L, Chowell G (2016). A generalized-growth model to characterize the early",
  "ascending phase of infectious disease outbreaks. Epidemics 15: 27-37.")
bff_ref_richards <- "Richards FJ (1959). A flexible growth function for empirical use. Journal of Experimental Botany 10(2): 290-301."
bff_ref_gompertz <- paste(
  "Gompertz B (1825). On the nature of the function expressive of the law of human mortality.",
  "Philosophical Transactions of the Royal Society of London 115: 513-583.")
bff_ref_kermack <- "Kermack WO, McKendrick AG (1927). A contribution to the mathematical theory of epidemics. Proceedings of the Royal Society A 115: 700-721."
bff_ref_sf1918 <- paste(
  "Chowell G, Nishiura H, Bettencourt LMA (2007). Comparative estimation of the reproduction number",
  "for pandemic influenza from daily case notification data. Journal of the Royal Society Interface 4: 155-166.")
bff_ref_growth_review <- paste(
  "Chowell G, Sattenspiel L, Bansal S, Viboud C (2016). Mathematical models to characterize early epidemic",
  "growth: A review. Physics of Life Reviews 18: 66-97.")
bff_ref_verhulst <- "Verhulst PF (1838). Notice sur la loi que la population suit dans son accroissement. Correspondance Mathematique et Physique 10: 113-121."
bff_ref_hethcote <- "Hethcote HW (2000). The mathematics of infectious diseases. SIAM Review 42(4): 599-653."
bff_ref_sird <- paste(
  "Anastassopoulou C, Russo L, Tsakris A, Siettos C (2020). Data-based analysis, modelling and forecasting",
  "of the COVID-19 outbreak. PLoS ONE 15(3): e0230405.")
bff_ref_undocumented <- paste(
  "Li R, Pei S, Chen B, et al. (2020). Substantial undocumented infection facilitates the rapid dissemination",
  "of novel coronavirus (SARS-CoV-2). Science 368: 489-493.")
bff_ref_rossmac <- paste(
  "Smith DL, Battle KE, Hay SI, Barker CM, Scott TW, McKenzie FE (2012). Ross, Macdonald, and a theory for the",
  "dynamics and control of mosquito-transmitted pathogens. PLoS Pathogens 8(4): e1002588.")

#------------------------------------------------------------------------------#
# The library ------------------------------------------------------------------
#------------------------------------------------------------------------------#

bff_model_library <- function() {

  growth_note_C0 <- "C(0) is set to the first observation of the fitted series (changed automatically when data are loaded)."
  pop_note <- "N is the population size of your setting: replace the placeholder value before fitting."

  models <- list(

    #--------------------------------------------------------------------------#
    EXP = bff_model_def(
      id = "EXP", title = "Exponential growth (EXP)", family = "Phenomenological growth models",
      tagline = "Constant per-capita growth: the simplest description of the early phase of an outbreak.",
      description = paste(
        "The cumulative number of cases C(t) grows at a constant per-capita rate r, so incidence",
        "increases exponentially. The model has a single parameter and is appropriate only while the",
        "epidemic is in its early, unconstrained phase; it will over-predict as soon as growth slows down.",
        "It is the natural baseline against which the other growth models are compared."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "intrinsic growth rate (per time unit)", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C"),
      composites = list(bff_model_composite("Td", "\\frac{\\log(2)}{r}", "doubling time of the epidemic")),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Early phase of an outbreak (first weeks), before any slow-down is visible.",
                      "As a baseline to compare with the GLM, Richards and Gompertz models."),
      assumptions = c("Unlimited susceptible population: no saturation.",
                      "Constant growth rate over the calibration period."),
      references = c(bff_ref_ggm, bff_ref_primer, bff_ref_pii, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    GLM = bff_model_def(
      id = "GLM", title = "Generalized logistic growth (GLM)", family = "Phenomenological growth models",
      tagline = "Logistic growth with a deceleration-of-growth parameter p: from sub-exponential to exponential early growth.",
      description = paste(
        "The generalized logistic model extends the logistic equation with the parameter p (0 <= p <= 1)",
        "that controls the early growth profile: p = 1 gives exponential early growth, p = 0 constant",
        "incidence (linear growth of C) and intermediate values give polynomial (sub-exponential) growth.",
        "Growth slows down as C approaches the final epidemic size K. It is widely used for short-term",
        "forecasting of single-wave outbreaks."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "growth rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("p", "p", "deceleration of growth (0 = constant incidence, 1 = exponential)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("K", "K", "final epidemic size (carrying capacity)", prior = "Normal", args = c(10000, 10000), trunc = TRUE, lb = "0",
                        scale_from = "total_cases", note = "The prior of K is centred on twice the total number of cases in the data (weakly informative)."),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C^p*(1-C/K)"),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Single-wave outbreaks when the data cover the growth phase and ideally the start of the slow-down.",
                      "Forecasting the final size and the peak timing without mechanistic assumptions."),
      assumptions = c("A single epidemic wave with a smooth deceleration.",
                      "K is poorly identified while the data are still in the early growth phase (wide posterior)."),
      references = c(bff_ref_ggm, bff_ref_primer, bff_ref_pii, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    Richards = bff_model_def(
      id = "Richards", title = "Richards growth model", family = "Phenomenological growth models",
      tagline = "Logistic-type growth with an extra shape parameter a that controls the asymmetry of the curve.",
      description = paste(
        "The Richards model generalizes the logistic equation with the exponent a, which measures the",
        "deviation from the symmetric S-shaped logistic curve (a = 1 gives the logistic model). It is a",
        "flexible three-parameter model for single-wave outbreaks and has been used extensively to",
        "describe SARS, dengue and influenza epidemics."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "growth rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("a", "a", "shape parameter (deviation from the logistic curve; a = 1 is logistic)", prior = "Normal", args = c(1, 1), trunc = TRUE, lb = "0"),
        bff_model_param("K", "K", "final epidemic size", prior = "Normal", args = c(10000, 10000), trunc = TRUE, lb = "0",
                        scale_from = "total_cases", note = "The prior of K is centred on twice the total number of cases in the data (weakly informative)."),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C*(1-(C/K)^a)"),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Single-wave outbreaks with an asymmetric epidemic curve (slow or fast approach to the final size).",
                      "When the logistic model fits poorly around the peak."),
      assumptions = c("A single epidemic wave.", "r and a can be strongly correlated: inspect the posterior correlations."),
      references = c(bff_ref_richards, bff_ref_primer, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    Gompertz = bff_model_def(
      id = "Gompertz", title = "Gompertz growth model", family = "Phenomenological growth models",
      tagline = "Growth rate decreasing exponentially with the logarithm of the cumulative count.",
      description = paste(
        "In the Gompertz model the per-capita growth rate decreases proportionally to log(K/C):",
        "growth is fastest at the start and slows down smoothly towards the final size K, producing an",
        "asymmetric curve whose inflection point occurs at C = K/e (about 37% of the final size).",
        "It is a classical alternative to the logistic family."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "growth rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("K", "K", "final epidemic size", prior = "Normal", args = c(10000, 10000), trunc = TRUE, lb = "0",
                        scale_from = "total_cases", note = "The prior of K is centred on twice the total number of cases in the data (weakly informative)."),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0) (must be > 0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C*log(K/C)"),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Single-wave outbreaks with an early peak followed by a long tail.",
                      "Comparison with the logistic-type models (GLM, Richards) through the model-fit metrics."),
      assumptions = c("C(0) must be strictly positive (the equation uses log(K/C)).",
                      "A single epidemic wave."),
      references = c(bff_ref_gompertz, bff_ref_primer, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SIR = bff_model_def(
      id = "SIR", title = "SIR model", family = "Compartmental (mechanistic) models",
      tagline = "Susceptible - Infectious - Recovered, with a cumulative-cases compartment for fitting incidence.",
      description = paste(
        "The classical Kermack-McKendrick model: susceptible individuals (S) become infectious (I) at rate",
        "beta S I / N and recover (R) at rate gamma. The auxiliary compartment C counts the cumulative",
        "number of new infections, so that its derivative dC/dt (new cases per time unit) can be compared",
        "with incidence data. The basic reproduction number R0 = beta / gamma is reported as a composite",
        "quantity."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("R", "recovered (removed) individuals", ic = "0"),
        bff_model_state("C", "cumulative number of cases (for fitting)", ic = "i_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\gamma", "gamma", "recovery rate (1 / infectious period)", fixed = TRUE, value = round(1 / 4.1, 4), lb = "0",
                        note = "Fixed to 1/4.1 per day (infectious period of 4.1 days, 1918 influenza). Change it for your disease, or set it to 'Estimate'."),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series; can also be estimated (choose 'Estimate' and give it a prior).")),
      equations = list(S = "-beta*S*I/N", I = "beta*S*I/N - gamma*I", R = "gamma*I", C = "beta*S*I/N"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New infections per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Directly transmitted infections without a relevant latent period.",
                      "Estimating the transmission rate and R0 from incidence data in a closed population."),
      assumptions = c("Homogeneous mixing, closed population (no births, deaths or migration).",
                      "Permanent immunity after recovery; constant transmission rate (see step 6 for time-dependent beta)."),
      references = c(bff_ref_kermack, bff_ref_pii, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SEIR = bff_model_def(
      id = "SEIR", title = "SEIR model", family = "Compartmental (mechanistic) models",
      tagline = "SIR with a latent (exposed) stage; the model of the BayesianFitForecast tutorial (1918 influenza, San Francisco).",
      description = paste(
        "Susceptible individuals (S) are infected at rate beta S I / N and enter a latent stage (E) before",
        "becoming infectious (I) at rate kappa; infectious individuals recover (R) at rate gamma. The",
        "compartment C accumulates the newly symptomatic / reported cases (rho kappa E, with reporting rate",
        "rho) and its derivative is compared with the incidence data. This is the model used in the",
        "BayesianFitForecast paper for the 1918 influenza pandemic in San Francisco."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("E", "exposed (latent) individuals", ic = "0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("R", "recovered individuals", ic = "0"),
        bff_model_state("C", "cumulative reported cases (for fitting)", ic = "i_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\kappa", "kappa", "rate of progression from exposed to infectious (1 / latent period)", fixed = TRUE, value = round(1 / 1.9, 4), lb = "0",
                        note = "Fixed to 1/1.9 per day (latent period of 1.9 days, 1918 influenza)."),
        bff_model_param("\\gamma", "gamma", "recovery rate (1 / infectious period)", fixed = TRUE, value = round(1 / 4.1, 4), lb = "0",
                        note = "Fixed to 1/4.1 per day (infectious period of 4.1 days, 1918 influenza)."),
        bff_model_param("\\rho", "rho", "reporting rate (fraction of infections that are reported)", fixed = TRUE, value = 1, lb = "0", ub = "1",
                        note = "Fixed to 1 (all cases reported); estimate it with a uniform(0, 1) prior when under-reporting is expected."),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*S*I/N", E = "beta*S*I/N - kappa*E", I = "kappa*E - gamma*I", R = "gamma*I", C = "rho*kappa*E"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New reported cases per time unit (dC/dt = rho kappa E) are compared with the first data column.",
      when_to_use = c("Infections with a latent period (influenza, COVID-19, measles, ...).",
                      "Reproducing the San Francisco 1918 tutorial example (N = 550000, kappa = 1/1.9, gamma = 1/4.1)."),
      assumptions = c("Homogeneous mixing, closed population, permanent immunity.",
                      "The latent and infectious periods are exponentially distributed."),
      references = c(bff_ref_sf1918, bff_ref_toolbox, bff_ref_pii)
    ),

    #--------------------------------------------------------------------------#
    SEIRD = bff_model_def(
      id = "SEIRD", title = "SEIRD model", family = "Compartmental (mechanistic) models",
      tagline = "SEIR with disease-induced deaths; fitted to a time series of deaths (case fatality ratio rho).",
      description = paste(
        "The SEIRD model adds a deaths compartment (D) to the SEIR model: a fraction rho of the",
        "infectious individuals die (rate rho gamma I) while the rest recover (rate (1 - rho) gamma I).",
        "The derivative of D (new deaths per time unit) is compared with mortality data, which are often",
        "more reliable than case counts. This is the structure used in the BayesianFitForecast tutorial",
        "for the 1905-1906 plague epidemic in Bombay."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("E", "exposed (latent) individuals", ic = "0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("R", "recovered individuals", ic = "0"),
        bff_model_state("D", "cumulative deaths (for fitting)", ic = "d_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\kappa", "kappa", "rate of progression from exposed to infectious (1 / latent period)", fixed = TRUE, value = round(1 / 1.9, 4), lb = "0",
                        note = "Fixed to 1/1.9 per day; change it for your disease or estimate it."),
        bff_model_param("\\gamma", "gamma", "removal rate (1 / infectious period)", fixed = TRUE, value = round(1 / 4.1, 4), lb = "0",
                        note = "Fixed to 1/4.1 per day; change it for your disease or estimate it."),
        bff_model_param("\\rho", "rho", "case fatality ratio (fraction of infectious individuals who die)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series (deaths)."),
        bff_model_param("d_0", "d0", "initial cumulative deaths D(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*S*I/N", E = "beta*S*I/N - kappa*E", I = "kappa*E - gamma*I",
                       R = "(1-rho)*gamma*I", D = "rho*gamma*I"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New deaths per time unit (dD/dt = rho gamma I) are compared with the first data column (deaths).",
      when_to_use = c("Outbreaks for which deaths are recorded more reliably than cases (plague, Ebola, COVID-19 mortality).",
                      "Estimating transmission together with the case fatality ratio."),
      assumptions = c("Homogeneous mixing, closed population.",
                      "Deaths occur at the end of the infectious period; no delay between infection and death beyond the latent and infectious periods."),
      references = c(bff_ref_toolbox, bff_ref_kermack, bff_ref_pii)
    ),

    #--------------------------------------------------------------------------#
    GGM = bff_model_def(
      id = "GGM", title = "Generalized growth model (GGM)", family = "Phenomenological growth models",
      tagline = "Early epidemic growth with a deceleration parameter p: from constant incidence (p = 0) to exponential growth (p = 1).",
      description = paste(
        "The generalized growth model describes the ascending phase of an outbreak with two parameters:",
        "the growth rate r and the deceleration of growth p. p = 1 recovers exponential growth, p = 0 gives",
        "constant incidence (linear growth of C) and intermediate values polynomial (sub-exponential) growth,",
        "which is typical of spatially structured or contact-limited epidemics. The model has no saturation",
        "and is meant for the early growth phase only (typically the first few weeks)."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "growth rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("p", "p", "deceleration of growth (0 = constant incidence, 1 = exponential)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C^p"),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Characterizing the early growth profile of an outbreak (sub-exponential vs exponential).",
                      "Short-term forecasts during the growth phase, before any slow-down."),
      assumptions = c("No saturation: the model must not be used once incidence starts to decline.",
                      "r and p are correlated; p is estimated reliably only with several generations of data."),
      references = c(bff_ref_ggm, bff_ref_growth_review, bff_ref_pii, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    Logistic = bff_model_def(
      id = "Logistic", title = "Logistic growth model", family = "Phenomenological growth models",
      tagline = "The classical symmetric S-shaped epidemic curve with final size K.",
      description = paste(
        "The logistic (Verhulst) model is the simplest saturating growth model: exponential growth at",
        "rate r at the start, slowing down symmetrically as C approaches the final size K (the peak of",
        "incidence occurs at C = K/2). It is a special case of the generalized logistic model (p = 1) and of",
        "the Richards model (a = 1)."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "growth rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("K", "K", "final epidemic size", prior = "Normal", args = c(10000, 10000), trunc = TRUE, lb = "0",
                        scale_from = "total_cases", note = "The prior of K is centred on twice the total number of cases in the data (weakly informative)."),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C*(1-C/K)"),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Single-wave outbreaks with a roughly symmetric epidemic curve.",
                      "A two-parameter baseline for the GLM, Richards and GRM models."),
      assumptions = c("Exponential early growth and a symmetric curve around the peak.",
                      "K is poorly identified while the data are still in the early growth phase."),
      references = c(bff_ref_verhulst, bff_ref_primer, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    GRM = bff_model_def(
      id = "GRM", title = "Generalized Richards model (GRM)", family = "Phenomenological growth models",
      tagline = "Richards growth with sub-exponential early growth (deceleration p) and asymmetry (a).",
      description = paste(
        "The generalized Richards model combines the early-growth flexibility of the GGM (parameter p)",
        "with the asymmetric saturation of the Richards model (parameter a and final size K). It is one of",
        "the most used phenomenological models for single-wave outbreaks (Ebola, Zika, COVID-19), at the",
        "price of weaker identifiability: with short time series the four parameters are strongly correlated."),
      states = list(
        bff_model_state("C", "cumulative number of cases", ic = "C_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("r", "r", "growth rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("p", "p", "deceleration of growth (0 = constant incidence, 1 = exponential)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("a", "a", "shape parameter (deviation from the logistic curve; a = 1 is logistic)", prior = "Normal", args = c(1, 1), trunc = TRUE, lb = "0"),
        bff_model_param("K", "K", "final epidemic size", prior = "Normal", args = c(10000, 10000), trunc = TRUE, lb = "0",
                        scale_from = "total_cases", note = "The prior of K is centred on twice the total number of cases in the data (weakly informative)."),
        bff_model_param("C_0", "C0", "initial cumulative cases C(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = growth_note_C0)),
      equations = list(C = "r*C^p*(1-(C/K)^a)"),
      fitted_text = "New cases per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Single-wave outbreaks that show sub-exponential early growth and an asymmetric decline.",
                      "When the GLM or the Richards model alone fits poorly."),
      assumptions = c("A single epidemic wave.",
                      "Four correlated parameters: use informative priors or fix p or a when the data are limited."),
      references = c(bff_ref_growth_review, bff_ref_primer, bff_ref_richards, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SIS = bff_model_def(
      id = "SIS", title = "SIS model", family = "Compartmental (mechanistic) models",
      tagline = "Susceptible - Infectious - Susceptible: no immunity after infection (endemic infections).",
      description = paste(
        "In the SIS model infectious individuals return to the susceptible class after recovery, so the",
        "infection can persist at an endemic level (I* = N (1 - 1/R0) when R0 > 1). It is the standard",
        "model for infections that confer no lasting immunity (many sexually transmitted infections, the",
        "common cold, some bacterial infections). The compartment C counts the cumulative new infections",
        "so that incidence data can be fitted."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("C", "cumulative number of infections (for fitting)", ic = "i_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\gamma", "gamma", "recovery rate (1 / infectious period)", fixed = TRUE, value = round(1 / 4.1, 4), lb = "0",
                        note = "Fixed to 1/4.1 per day; change it for your infection or set it to 'Estimate'."),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*S*I/N + gamma*I", I = "beta*S*I/N - gamma*I", C = "beta*S*I/N"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New infections per time unit (dC/dt) are compared with the first data column (prevalence I can be fitted instead: set 'Fit to data' on I with 'Fit to derivative' = No).",
      when_to_use = c("Infections without lasting immunity that settle at an endemic level.",
                      "Prevalence surveys (fit I directly) or incidence data (fit dC/dt)."),
      assumptions = c("Homogeneous mixing, closed population, no immunity at all.",
                      "With incidence data only, beta and gamma are hard to separate: keep gamma fixed."),
      references = c(bff_ref_hethcote, bff_ref_kermack, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SIRS = bff_model_def(
      id = "SIRS", title = "SIRS model", family = "Compartmental (mechanistic) models",
      tagline = "SIR with waning immunity: recovered individuals become susceptible again at rate omega.",
      description = paste(
        "The SIRS model adds the loss of immunity (rate omega, 1 / duration of immunity) to the SIR model,",
        "which allows recurrent epidemics and an endemic equilibrium. It is used for infections with",
        "temporary immunity such as seasonal influenza, RSV, pertussis or seasonal coronaviruses. C",
        "accumulates the new infections for fitting incidence data."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("R", "recovered (temporarily immune) individuals", ic = "0"),
        bff_model_state("C", "cumulative number of infections (for fitting)", ic = "i_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\gamma", "gamma", "recovery rate (1 / infectious period)", fixed = TRUE, value = round(1 / 4.1, 4), lb = "0",
                        note = "Fixed to 1/4.1 per day; change it for your infection or set it to 'Estimate'."),
        bff_model_param("\\omega", "omega", "rate of loss of immunity (1 / duration of immunity)", fixed = TRUE, value = 0.01, lb = "0",
                        note = "Fixed to 1/100 per day (immunity lasting about 100 days on average); change it for your infection."),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*S*I/N + omega*R", I = "beta*S*I/N - gamma*I", R = "gamma*I - omega*R", C = "beta*S*I/N"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New infections per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Infections with temporary immunity and recurrent waves (seasonal influenza, RSV, seasonal coronaviruses).",
                      "Long time series covering more than one wave."),
      assumptions = c("Homogeneous mixing, closed population, exponentially distributed immunity.",
                      "omega is not identifiable from a single wave: keep it fixed unless the data cover several waves."),
      references = c(bff_ref_hethcote, bff_ref_kermack, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SIRD = bff_model_def(
      id = "SIRD", title = "SIRD model", family = "Compartmental (mechanistic) models",
      tagline = "SIR with disease-induced deaths, fitted to a time series of deaths (case fatality ratio rho).",
      description = paste(
        "The SIRD model splits the removal of infectious individuals into recovery (rate (1 - rho) gamma)",
        "and death (rate rho gamma). The derivative of D (new deaths per time unit) is compared with",
        "mortality data, which are often more reliable than case counts. It was used extensively in the",
        "early phase of the COVID-19 pandemic; it is the SEIRD model without the latent stage."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("R", "recovered individuals", ic = "0"),
        bff_model_state("D", "cumulative deaths (for fitting)", ic = "d_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\gamma", "gamma", "removal rate (1 / infectious period)", fixed = TRUE, value = round(1 / 4.1, 4), lb = "0",
                        note = "Fixed to 1/4.1 per day; change it for your disease or estimate it."),
        bff_model_param("\\rho", "rho", "case fatality ratio (fraction of infectious individuals who die)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series (deaths)."),
        bff_model_param("d_0", "d0", "initial cumulative deaths D(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*S*I/N", I = "beta*S*I/N - gamma*I", R = "(1-rho)*gamma*I", D = "rho*gamma*I"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New deaths per time unit (dD/dt = rho gamma I) are compared with the first data column (deaths).",
      when_to_use = c("Outbreaks for which deaths are recorded more reliably than cases.",
                      "A simpler alternative to SEIRD when the latent period is short or unknown."),
      assumptions = c("Homogeneous mixing, closed population, no latent period.",
                      "beta and rho are only weakly identified from deaths alone: fix gamma and use an informative prior for rho when possible."),
      references = c(bff_ref_sird, bff_ref_kermack, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SEIUR = bff_model_def(
      id = "SEIUR", title = "SEIUR model (reported and unreported infections)", family = "Compartmental (mechanistic) models",
      tagline = "SEIR in which only a fraction rho of the infections is reported; unreported infections also transmit.",
      description = paste(
        "Exposed individuals become infectious and are either reported (I, fraction rho) or unreported (U,",
        "fraction 1 - rho); both classes transmit and recover at rate gamma. The compartment C accumulates",
        "the reported infections only, so the model can be fitted to reported case counts while accounting",
        "for under-ascertainment. This is the structure of the COVID-19 example of the BayesianFitForecast",
        "tutorial (SEIURC) and of the SEIR-UR / SEIAR models of the identifiability paper."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("E", "exposed (latent) individuals", ic = "0"),
        bff_model_state("I", "reported infectious individuals", ic = "i_0"),
        bff_model_state("U", "unreported infectious individuals", ic = "0"),
        bff_model_state("R", "recovered individuals", ic = "0"),
        bff_model_state("C", "cumulative reported cases (for fitting)", ic = "i_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\kappa", "kappa", "rate of progression from exposed to infectious (1 / latent period)", fixed = TRUE, value = 0.25, lb = "0",
                        note = "Fixed to 1/4 per day (latent period of 4 days, COVID-19-like); change it for your disease."),
        bff_model_param("\\gamma", "gamma", "recovery rate (1 / infectious period)", fixed = TRUE, value = 0.2, lb = "0",
                        note = "Fixed to 1/5 per day (infectious period of 5 days); change it for your disease."),
        bff_model_param("\\rho", "rho", "reporting rate (fraction of infections that are reported)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of reported infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*(I+U)*S/N", E = "beta*(I+U)*S/N - kappa*E", I = "kappa*rho*E - gamma*I",
                       U = "kappa*(1-rho)*E - gamma*U", R = "gamma*(I+U)", C = "kappa*rho*E"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New reported cases per time unit (dC/dt = kappa rho E) are compared with the first data column.",
      when_to_use = c("Outbreaks with substantial under-reporting or asymptomatic transmission (COVID-19, influenza).",
                      "Estimating the reporting rate together with transmission."),
      assumptions = c("Reported and unreported infections are equally infectious and recover at the same rate.",
                      "rho and beta are correlated; with case counts only, rho is identified mainly through the fixed N and the final size."),
      references = c(bff_ref_undocumented, bff_ref_pii, bff_ref_toolbox)
    ),

    #--------------------------------------------------------------------------#
    SEIHR = bff_model_def(
      id = "SEIHR", title = "SEIHR model (hospital admissions)", family = "Compartmental (mechanistic) models",
      tagline = "SEIR in which a fraction eta of the infectious individuals is hospitalized; fitted to hospital admissions.",
      description = paste(
        "At the end of the infectious period a fraction eta of the infectious individuals is hospitalized",
        "(cumulative admissions H) and the rest recover (R). The derivative of H (new admissions per time",
        "unit) is compared with hospital-admission data, which are less affected by testing intensity than",
        "case counts. This is the hospitalization structure of the Switzerland example of the",
        "BayesianFitForecast tutorial."),
      states = list(
        bff_model_state("S", "susceptible individuals", ic = "N-i_0"),
        bff_model_state("E", "exposed (latent) individuals", ic = "0"),
        bff_model_state("I", "infectious individuals", ic = "i_0"),
        bff_model_state("R", "recovered individuals (not hospitalized)", ic = "0"),
        bff_model_state("H", "cumulative hospital admissions (for fitting)", ic = "h_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta", "beta", "transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\kappa", "kappa", "rate of progression from exposed to infectious (1 / latent period)", fixed = TRUE, value = 0.25, lb = "0",
                        note = "Fixed to 1/4 per day; change it for your disease."),
        bff_model_param("\\gamma", "gamma", "removal rate (1 / infectious period)", fixed = TRUE, value = 0.2, lb = "0",
                        note = "Fixed to 1/5 per day; change it for your disease."),
        bff_model_param("\\eta", "eta", "hospitalization fraction (fraction of infectious individuals admitted to hospital)", prior = "Uniform", args = c(0, 1), trunc = FALSE, lb = "0", ub = "1"),
        bff_model_param("N", "N", "population size", fixed = TRUE, value = 1e6, lb = "0", note = pop_note),
        bff_model_param("i_0", "i0", "initial number of infectious individuals", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series (admissions); increase it when admissions are a small fraction of infections."),
        bff_model_param("h_0", "h0", "initial cumulative admissions H(0)", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series.")),
      equations = list(S = "-beta*S*I/N", E = "beta*S*I/N - kappa*E", I = "kappa*E - gamma*I",
                       R = "(1-eta)*gamma*I", H = "eta*gamma*I"),
      composites = list(bff_model_composite("R0", "\\frac{\\beta}{\\gamma}", "basic reproduction number")),
      fitted_text = "New hospital admissions per time unit (dH/dt = eta gamma I) are compared with the first data column.",
      when_to_use = c("Hospital-admission time series (COVID-19, influenza).",
                      "Estimating transmission when case counts are unreliable."),
      assumptions = c("Admission happens at the end of the infectious period; hospitalized individuals no longer transmit.",
                      "beta and eta are only weakly identified from admissions alone: fix eta (from the literature) when possible."),
      references = c(bff_ref_toolbox, bff_ref_pii, bff_ref_kermack)
    ),

    #--------------------------------------------------------------------------#
    RossMacdonald = bff_model_def(
      id = "RossMacdonald", title = "Ross-Macdonald model (host-vector)", family = "Vector-borne (host-vector) models",
      tagline = "SIR humans coupled with SI mosquitoes: the classical model of mosquito-borne infections (malaria, dengue, Zika).",
      description = paste(
        "Humans (S_h, I_h, R_h) are infected by bites of infectious mosquitoes at rate beta_h S_h I_v / N_h",
        "and recover at rate gamma; mosquitoes (S_v, I_v) are infected by biting infectious humans at rate",
        "beta_v S_v I_h / N_h and die at rate mu_v (they are replaced by susceptible mosquitoes, so the vector",
        "population N_v is constant). The transmission rates combine the biting rate with the probabilities",
        "of transmission per bite. C accumulates the new human infections for fitting incidence data."),
      states = list(
        bff_model_state("Sh", "susceptible humans", ic = "N_h-i_0"),
        bff_model_state("Ih", "infectious humans", ic = "i_0"),
        bff_model_state("Rh", "recovered humans", ic = "0"),
        bff_model_state("Sv", "susceptible mosquitoes", ic = "N_v-v_0"),
        bff_model_state("Iv", "infectious mosquitoes", ic = "v_0"),
        bff_model_state("C", "cumulative human cases (for fitting)", ic = "i_0", fit = TRUE, series = 1L, diff = TRUE)),
      params = list(
        bff_model_param("\\beta_h", "betah", "mosquito-to-human transmission rate (bites x transmission probability)", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\beta_v", "betav", "human-to-mosquito transmission rate", prior = "Normal", args = c(0.5, 1), trunc = TRUE, lb = "0"),
        bff_model_param("\\gamma", "gamma", "human recovery rate (1 / infectious period)", fixed = TRUE, value = round(1 / 7, 4), lb = "0",
                        note = "Fixed to 1/7 per day (about one week of viraemia, dengue-like); change it for your infection."),
        bff_model_param("\\mu_v", "muv", "mosquito mortality rate (1 / mosquito lifespan)", fixed = TRUE, value = round(1 / 14, 4), lb = "0",
                        note = "Fixed to 1/14 per day (two-week lifespan); change it for your vector."),
        bff_model_param("N_h", "Nh", "human population size", fixed = TRUE, value = 1e6, lb = "0", note = "N_h is the human population of your setting: replace the placeholder value."),
        bff_model_param("N_v", "Nv", "mosquito population size", fixed = TRUE, value = 2e6, lb = "0",
                        note = "Usually unknown: fix it to a plausible multiple of the human population (only the product beta_h beta_v N_v is identified)."),
        bff_model_param("i_0", "i0", "initial number of infectious humans", fixed = TRUE, value = 1, value_from = "first_obs", lb = "0",
                        note = "Set to the first observation of the fitted series."),
        bff_model_param("v_0", "v0", "initial number of infectious mosquitoes", fixed = TRUE, value = 10, lb = "0",
                        note = "Rarely known: a small positive number, or estimate it with a wide prior.")),
      equations = list(Sh = "-betah*Sh*Iv/Nh", Ih = "betah*Sh*Iv/Nh - gamma*Ih", Rh = "gamma*Ih",
                       Sv = "muv*Nv - betav*Sv*Ih/Nh - muv*Sv", Iv = "betav*Sv*Ih/Nh - muv*Iv", C = "betah*Sh*Iv/Nh"),
      composites = list(bff_model_composite("R0", "\\sqrt{\\frac{\\beta_h \\beta_v N_v}{\\gamma \\mu_v N_h}}", "basic reproduction number (host-vector)")),
      fitted_text = "New human infections per time unit (dC/dt) are compared with the first data column.",
      when_to_use = c("Mosquito-borne infections: dengue, Zika, chikungunya, malaria (single season / outbreak).",
                      "When the vector dynamics (short mosquito lifespan) matter for the shape of the epidemic."),
      assumptions = c("Constant mosquito population, no extrinsic incubation period, homogeneous biting.",
                      "The vector parameters (mu_v, N_v) are not identifiable from human cases: keep them fixed; beta_h and beta_v are only identified through their product."),
      references = c(bff_ref_rossmac, bff_ref_hethcote, bff_ref_toolbox)
    )
  )

  # Order shown in the library (growth models, compartmental models, vector-borne)
  models[c("EXP", "GGM", "Logistic", "GLM", "Richards", "GRM", "Gompertz",
           "SIS", "SIR", "SIRS", "SIRD", "SEIR", "SEIRD", "SEIUR", "SEIHR", "RossMacdonald")]
}

#------------------------------------------------------------------------------#
# Helpers ----------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Named vector of choices for a picker (grouped by family)
bff_model_choices <- function(models = bff_model_library()) {
  fam <- vapply(models, function(m) m$family, character(1))
  out <- list()
  for (f in unique(fam)) {
    ids <- names(models)[fam == f]
    out[[f]] <- stats::setNames(ids, vapply(models[ids], function(m) m$title, character(1)))
  }
  out
}

# Internal name of a parameter (same convention as to_internal_name in app.R)
bff_model_param_names <- function(model) vapply(model$params, function(p) p$name, character(1))
bff_model_state_names <- function(model) vapply(model$states, function(s) s$name, character(1))

# Initial-condition expression with internal identifiers ("N-i_0" -> "N-i0")
bff_model_ic_internal <- function(ic) {
  x <- gsub("\\\\", "", ic)
  x <- gsub("[{}]", "", x)
  gsub("_", "", x)
}

# Parameters that appear in the initial conditions (they get automatic rows in the form)
bff_model_ic_params <- function(model) {
  ics <- vapply(model$states, function(s) bff_model_ic_internal(s$ic), character(1))
  toks <- unique(unlist(regmatches(ics, gregexpr("[A-Za-z][A-Za-z0-9_]*", ics))))
  intersect(bff_model_param_names(model), toks)
}

# Fill the data-dependent defaults (first observation, total cases)
bff_model_resolve_defaults <- function(model, data = NULL) {
  first_obs <- NA_real_; total <- NA_real_
  if (!is.null(data) && ncol(data) >= 2) {
    y <- suppressWarnings(as.numeric(data[[2]]))
    y <- y[!is.na(y)]
    if (length(y) > 0) {
      first_obs <- max(1, round(y[1]))
      total <- sum(pmax(y, 0))
    }
  }
  model$params <- lapply(model$params, function(p) {
    if (identical(p$value_from, "first_obs") && !is.na(first_obs)) p$value <- first_obs
    if (identical(p$scale_from, "total_cases") && !is.na(total) && total > 0) {
      k0 <- signif(2 * total, 2)
      k0 <- max(k0, 100)
      p$args <- c(k0, k0)
    }
    p
  })
  model
}

# Lookup table (name -> key / kind / display) used to turn the equation text into tokens.
#   state_keys / param_keys: named character vectors (internal name -> row key)
bff_model_lookup <- function(model, state_keys = NULL, param_keys = NULL) {
  s_names <- bff_model_state_names(model)
  p_names <- bff_model_param_names(model)
  if (is.null(state_keys)) state_keys <- stats::setNames(paste0("state:", s_names), s_names)
  if (is.null(param_keys)) param_keys <- stats::setNames(paste0("param:", p_names), p_names)
  rbind(
    data.frame(name = s_names, key = unname(state_keys[s_names]), kind = "state", display = s_names, stringsAsFactors = FALSE),
    data.frame(name = p_names, key = unname(param_keys[p_names]), kind = "param",
               display = vapply(model$params, function(p) p$latex, character(1)), stringsAsFactors = FALSE)
  )
}

# Static catalog (for rendering outside the Shiny module)
bff_model_static_catalog <- function(model, lookup = bff_model_lookup(model)) {
  st <- lookup[lookup$kind == "state", , drop = FALSE]
  pa <- lookup[lookup$kind == "param", , drop = FALSE]
  rbind(
    ode_builder_named_vector_to_tokens(data.frame(key = st$key, display = st$display, stringsAsFactors = FALSE), "state"),
    ode_builder_named_vector_to_tokens(data.frame(key = pa$key, display = pa$display, stringsAsFactors = FALSE), "param"),
    ode_builder_operator_tokens()
  )
}

# Tokens of every equation (named by state name), resolved against the catalog
bff_model_equation_tokens <- function(model, lookup = bff_model_lookup(model)) {
  catalog <- bff_model_static_catalog(model, lookup)
  out <- lapply(bff_model_state_names(model), function(s) {
    toks <- ode_builder_tokenize_text(model$equations[[s]] %||% "", lookup)
    ode_builder_resolve_tokens(toks, catalog)
  })
  names(out) <- bff_model_state_names(model)
  out
}

# LaTeX of the right-hand sides (named by state name); compact = slash fractions
bff_model_equations_latex <- function(model, compact = FALSE) {
  lookup <- bff_model_lookup(model)
  catalog <- bff_model_static_catalog(model, lookup)
  toks <- bff_model_equation_tokens(model, lookup)
  vapply(toks, function(tk) ode_builder_tokens_to_math_latex(tk, catalog, compact = compact)$latex, character(1))
}

# One-line equations for the library cards: "dS/dt = -\beta S I/N" ... (one inline
# math span per equation, so the browser wraps between equations)
bff_model_equations_inline_ui <- function(model) {
  rhs <- bff_model_equations_latex(model, compact = TRUE)
  shiny::div(class = "bff-model-eq-line",
             lapply(seq_along(rhs), function(i) {
               shiny::tags$span(class = "bff-model-eq-inline",
                                paste0("\\(d", names(rhs)[i], "/dt = ", rhs[[i]], if (i < length(rhs)) "," else "", "\\)"))
             }))
}

# One LaTeX block with all equations (aligned)
bff_model_system_latex <- function(model) {
  rhs <- bff_model_equations_latex(model)
  lines <- sprintf("\\frac{d%s}{dt} &= %s", names(rhs), rhs)
  paste0("\\begin{aligned}", paste(lines, collapse = " \\\\[4pt] "), "\\end{aligned}")
}

# Human-readable default of a parameter ("normal(0.5, 1) truncated at 0" / "fixed = 0.244")
bff_model_param_default_text <- function(p) {
  if (isTRUE(p$fixed)) {
    return(sprintf("fixed = %s", format(p$value, big.mark = ",", scientific = FALSE, trim = TRUE)))
  }
  fam <- tolower(p$prior %||% "normal")
  args <- paste(format(p$args, trim = TRUE, scientific = FALSE, big.mark = ","), collapse = ", ")
  paste0("estimated, prior ", fam, "(", args, ")", if (isTRUE(p$trunc)) " truncated at 0" else "")
}

bff_model_bounds_text <- function(p) {
  lb <- p$lb %||% "NA"; ub <- p$ub %||% "NA"
  if (identical(lb, "NA") && identical(ub, "NA")) return("none")
  paste0("[", if (identical(lb, "NA")) "-Inf" else lb, ", ", if (identical(ub, "NA")) "Inf" else ub, "]")
}

# Initial-condition LaTeX for display ("N-i_0" -> "N - i_0")
bff_model_ic_latex <- function(ic) {
  gsub("\\s*([-+])\\s*", " \\1 ", ic)
}

#------------------------------------------------------------------------------#
# Model card (Shiny UI) --------------------------------------------------------
#------------------------------------------------------------------------------#
# compact = TRUE gives the summary card shown in the library grid; FALSE the     #
# full documentation (modal / report).                                          #
#------------------------------------------------------------------------------#
bff_model_card_ui <- function(model, compact = TRUE, data = NULL) {
  model <- bff_model_resolve_defaults(model, data)
  eq <- bff_model_system_latex(model)
  n_est <- sum(!vapply(model$params, function(p) p$fixed, logical(1)))
  n_fix <- length(model$params) - n_est

  math_block <- shiny::withMathJax(shiny::div(class = "bff-model-equations", paste0("\\[", eq, "\\]")))

  state_table <- shiny::tags$table(
    class = "table table-condensed bff-model-table",
    shiny::tags$thead(shiny::tags$tr(shiny::tags$th("State"), shiny::tags$th("Meaning"), shiny::tags$th("Initial condition"), shiny::tags$th("Fitted"))),
    shiny::tags$tbody(lapply(model$states, function(s) {
      shiny::tags$tr(
        shiny::tags$td(shiny::HTML(paste0("\\(", s$name, "\\)"))),
        shiny::tags$td(s$meaning),
        shiny::tags$td(shiny::HTML(paste0("\\(", s$name, "(0) = ", bff_model_ic_latex(s$ic), "\\)"))),
        shiny::tags$td(if (s$fit) sprintf("yes: %s to data column %d", if (s$diff) paste0("d", s$name, "/dt") else s$name, s$series) else "no"))
    }))
  )

  param_table <- shiny::tags$table(
    class = "table table-condensed bff-model-table",
    shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Parameter"), shiny::tags$th("Meaning"), shiny::tags$th("Default"), shiny::tags$th("Bounds"))),
    shiny::tags$tbody(lapply(model$params, function(p) {
      shiny::tags$tr(
        shiny::tags$td(shiny::HTML(paste0("\\(", p$latex, "\\)"))),
        shiny::tags$td(p$meaning, if (!is.null(p$note)) shiny::tags$div(class = "bff-hint", p$note) else NULL),
        shiny::tags$td(bff_model_param_default_text(p)),
        shiny::tags$td(bff_model_bounds_text(p)))
    }))
  )

  composites_ui <- if (length(model$composites) > 0) {
    shiny::tags$p(shiny::strong("Composite quantities: "),
                  shiny::HTML(paste(vapply(model$composites, function(cp) {
                    sprintf("\\(%s = %s\\) (%s)", cp$name, cp$latex, cp$meaning)
                  }, character(1)), collapse = "; ")))
  } else NULL

  bullets <- function(title, items) {
    if (length(items) == 0) return(NULL)
    shiny::tagList(shiny::tags$p(shiny::strong(title)), shiny::tags$ul(lapply(items, shiny::tags$li)))
  }

  summary_line <- sprintf("%d state variable%s, %d parameter%s (%d estimated, %d fixed)%s. Error structure: %s.",
                          length(model$states), if (length(model$states) == 1) "" else "s",
                          length(model$params), if (length(model$params) == 1) "" else "s", n_est, n_fix,
                          if (length(model$composites) > 0) sprintf(", %d composite quantit%s", length(model$composites), if (length(model$composites) == 1) "y" else "ies") else "",
                          tolower(model$errstrc))

  if (compact) {
    # Library grid: title, equations and the two buttons only (details in the modal)
    return(shiny::withMathJax(shiny::div(
      class = "bff-model-card",
      shiny::div(class = "bff-model-card-head",
                 shiny::tags$h4(class = "bff-model-title", model$title),
                 shiny::div(class = "bff-model-card-actions",
                            shiny::tags$button(type = "button", class = "btn btn-primary btn-sm",
                                               onclick = sprintf("Shiny.setInputValue('use_model', {id: '%s', nonce: Math.random()}, {priority: 'event'});", model$id),
                                               shiny::icon("download"), " Use this model"),
                            shiny::tags$button(type = "button", class = "btn btn-default btn-sm",
                                               onclick = sprintf("Shiny.setInputValue('model_details', {id: '%s', nonce: Math.random()}, {priority: 'event'});", model$id),
                                               shiny::icon("book"), " Details & report"))),
      bff_model_equations_inline_ui(model)
    )))
  }

  shiny::withMathJax(shiny::div(
    class = "bff-model-report",
    shiny::tags$h3(model$title, " ", shiny::span(class = "label label-default", model$family)),
    shiny::tags$p(shiny::em(model$tagline)),
    shiny::tags$p(model$description),
    shiny::tags$h4("Equations"),
    math_block,
    shiny::tags$p(class = "bff-hint", summary_line),
    shiny::tags$h4("State variables and initial conditions"),
    state_table,
    shiny::tags$h4("Parameters and default priors"),
    param_table,
    composites_ui,
    shiny::tags$p(shiny::strong("Fitted quantity: "), model$fitted_text),
    bullets("When to use it", model$when_to_use),
    bullets("Assumptions and limitations", model$assumptions),
    bullets("Notes", model$notes),
    bullets("References", model$references)
  ))
}

#------------------------------------------------------------------------------#
# Model card (Markdown, for the run report) -------------------------------------
#------------------------------------------------------------------------------#
bff_model_card_markdown <- function(model, data = NULL, heading_level = 2) {
  model <- bff_model_resolve_defaults(model, data)
  h <- function(level, txt) paste0(strrep("#", heading_level + level - 1), " ", txt)
  rhs <- bff_model_equations_latex(model)
  eq_lines <- sprintf("\\frac{d%s}{dt} &= %s", names(rhs), rhs)
  md <- c(h(1, paste0("Model: ", model$title)), "",
          paste0("*", model$tagline, "*"), "",
          model$description, "",
          "$$", "\\begin{aligned}", paste0(eq_lines, " \\\\"), "\\end{aligned}", "$$", "",
          "| State | Meaning | Initial condition | Fitted |", "|---|---|---|---|")
  for (s in model$states) {
    md <- c(md, sprintf("| $%s$ | %s | $%s(0) = %s$ | %s |", s$name, s$meaning, s$name, bff_model_ic_latex(s$ic),
                        if (s$fit) sprintf("yes: %s to data column %d", if (s$diff) paste0("d", s$name, "/dt") else s$name, s$series) else "no"))
  }
  md <- c(md, "", "| Parameter | Meaning | Default | Bounds |", "|---|---|---|---|")
  for (p in model$params) {
    md <- c(md, sprintf("| $%s$ | %s%s | %s | %s |", p$latex, p$meaning,
                        if (!is.null(p$note)) paste0(" (", p$note, ")") else "",
                        bff_model_param_default_text(p), bff_model_bounds_text(p)))
  }
  if (length(model$composites) > 0) {
    md <- c(md, "", paste0("**Composite quantities:** ",
                           paste(vapply(model$composites, function(cp) sprintf("$%s = %s$ (%s)", cp$name, cp$latex, cp$meaning), character(1)), collapse = "; ")))
  }
  md <- c(md, "", paste0("**Fitted quantity:** ", model$fitted_text), "")
  add_list <- function(title, items) {
    if (length(items) == 0) return(character(0))
    c(paste0("**", title, "**"), "", paste0("* ", items), "")
  }
  md <- c(md, add_list("When to use it", model$when_to_use),
          add_list("Assumptions and limitations", model$assumptions),
          add_list("Notes", model$notes),
          add_list("References", model$references))
  md
}


#------------------------------------------------------------------------------#
# Description of the model actually fitted (from an options file) ---------------
#------------------------------------------------------------------------------#
# Renders the equations of a configuration (ode_system with varsN / paramsN      #
# placeholders) with the real names, plus the initial conditions, parameters    #
# and priors, so that the report of every run documents the model that was     #
# fitted, whether or not it came from the library.                              #
#------------------------------------------------------------------------------#
bff_options_model_info <- function(opt) {
  get_opt <- function(name, default = NULL) if (exists(name, envir = opt, inherits = FALSE)) get(name, envir = opt) else default

  vars <- as.character(get_opt("vars", character(0)))
  params <- as.character(get_opt("params", character(0)))
  params_display <- get_opt("params_display", NULL)
  if (is.null(params_display) || length(params_display) != length(params)) params_display <- vapply(params, bff_pretty_param_name, character(1))
  params_display <- as.character(params_display)
  paramsfix <- as.numeric(get_opt("paramsfix", rep(0, length(params))))
  Ic <- get_opt("Ic", NULL)
  fitting_index <- as.integer(get_opt("fitting_index", integer(0)))
  fitting_diff <- as.integer(get_opt("fitting_diff", rep(1L, length(fitting_index))))
  errstrc <- as.integer(get_opt("errstrc", 1L))
  composite <- get_opt("composite_expressions", list())
  tdp_display <- get_opt("time_dependent_display_names", character(0))
  model_template <- get_opt("model_template", NA)
  if (is.null(model_template) || length(model_template) != 1 || is.na(model_template)) model_template <- NULL

  # Equations: ode_system lines "diff_varK = <rhs>" -> rhs text
  ode <- as.character(get_opt("ode_system", ""))
  lines <- trimws(unlist(strsplit(ode, "\n", fixed = TRUE)))
  lines <- lines[grepl("^diff_var[0-9]+\\s*=", lines)]
  rhs_text <- rep("", length(vars))
  for (ln in lines) {
    k <- as.integer(sub("^diff_var([0-9]+).*$", "\\1", ln))
    if (!is.na(k) && k >= 1 && k <= length(vars)) rhs_text[k] <- trimws(sub("^diff_var[0-9]+\\s*=\\s*", "", ln))
  }

  lookup <- rbind(
    if (length(vars)) data.frame(name = paste0("vars", seq_along(vars)), key = paste0("vars", seq_along(vars)), kind = "state", display = vars, stringsAsFactors = FALSE),
    if (length(params)) data.frame(name = paste0("params", seq_along(params)), key = paste0("params", seq_along(params)), kind = "param", display = params_display, stringsAsFactors = FALSE)
  )
  tdp_names <- names(tdp_display)
  if (length(tdp_names) > 0) {
    lookup <- rbind(lookup, data.frame(name = tdp_names, key = tdp_names, kind = "tdp", display = unname(tdp_display), stringsAsFactors = FALSE))
  }
  catalog <- rbind(
    ode_builder_named_vector_to_tokens(data.frame(key = paste0("vars", seq_along(vars)), display = vars, stringsAsFactors = FALSE), "state"),
    ode_builder_named_vector_to_tokens(data.frame(key = paste0("params", seq_along(params)), display = params_display, stringsAsFactors = FALSE), "param"),
    if (length(tdp_names) > 0) ode_builder_token_row(unname(tdp_display), unname(tdp_display), tdp_names, "tdp") else NULL,
    ode_builder_operator_tokens()
  )
  render_rhs <- function(txt) {
    if (!nzchar(txt)) return("?")
    tryCatch({
      toks <- ode_builder_tokenize_text(txt, lookup)
      toks <- ode_builder_resolve_tokens(toks, catalog)
      ode_builder_tokens_to_math_latex(toks, catalog)$latex
    }, error = function(e) paste0("\\text{", gsub("_", "\\\\_", txt), "}"))
  }
  rhs_latex <- vapply(rhs_text, render_rhs, character(1), USE.NAMES = FALSE)
  eq_lines <- if (length(vars)) sprintf("\\frac{d%s}{dt} &= %s", vars, rhs_latex) else character(0)
  system_latex <- if (length(eq_lines)) paste0("\\begin{aligned}", paste(eq_lines, collapse = " \\\\[4pt] "), "\\end{aligned}") else ""

  ic_text <- if (!is.null(Ic) && length(Ic) == length(vars)) {
    vapply(seq_along(vars), function(i) {
      v <- Ic[[i]]
      sprintf("%s(0) = %s", vars[i], if (is.numeric(v)) format(v, big.mark = ",", scientific = FALSE) else as.character(v))
    }, character(1))
  } else character(0)

  fit_text <- if (length(fitting_index) > 0) {
    vapply(seq_along(fitting_index), function(j) {
      v <- if (fitting_index[j] >= 1 && fitting_index[j] <= length(vars)) vars[fitting_index[j]] else paste0("var", fitting_index[j])
      sprintf("data column %d: %s", j, if (isTRUE(fitting_diff[j] == 1L)) paste0("d", v, "/dt (new cases per time unit)") else v)
    }, character(1))
  } else character(0)

  param_table <- if (length(params)) {
    data.frame(
      Parameter = params_display,
      Internal = params,
      Status = ifelse(paramsfix == 1, "fixed", "estimated"),
      `Prior / value` = vapply(seq_along(params), function(i) {
        prior <- get_opt(paste0("params", i, "_prior"), NA)
        if (is.numeric(prior)) format(prior, big.mark = ",", scientific = FALSE) else as.character(prior)
      }, character(1)),
      Bounds = vapply(seq_along(params), function(i) {
        if (paramsfix[i] == 1) return("")
        lb <- get_opt(paste0("params", i, "_LB"), NA); ub <- get_opt(paste0("params", i, "_UB"), NA)
        paste0("[", if (is.null(lb) || is.na(lb)) "-Inf" else lb, ", ", if (is.null(ub) || is.na(ub)) "Inf" else ub, "]")
      }, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
  } else data.frame()

  err_text <- c("negative binomial", "normal", "Poisson")[errstrc]
  err_priors <- character(0)
  if (errstrc %in% c(1, 2) && length(fitting_index) > 0) {
    prefix <- if (errstrc == 1) "negbinerror" else "normalerror"
    err_priors <- vapply(seq_along(fitting_index), function(j) {
      v <- get_opt(paste0(prefix, j, "_prior"), NA)
      sprintf("series %d: %s", j, if (is.null(v) || is.na(v)) "?" else as.character(v))
    }, character(1))
  }

  list(vars = vars, params = params, params_display = params_display, rhs_latex = rhs_latex,
       system_latex = system_latex, ic_text = ic_text, fit_text = fit_text, param_table = param_table,
       composites = composite, errstrc = errstrc, err_text = err_text, err_priors = err_priors,
       model_template = model_template, tdp_display = tdp_display)
}

# LaTeX display name of an internal parameter name written in the toolbox
# convention ("beta" -> "\\beta", "beta0" -> "\\beta_0", "t_int" -> "t_{int}")
bff_pretty_param_name <- function(name) {
  greek <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
             "mu", "nu", "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega")
  name <- as.character(name)
  m <- regmatches(name, regexec("^([A-Za-z]+?)([0-9]+)$", name))[[1]]
  base <- if (length(m) == 3) m[2] else name
  sub_ <- if (length(m) == 3) m[3] else ""
  if (grepl("_", base)) {
    parts <- strsplit(base, "_", fixed = TRUE)[[1]]
    base <- parts[1]; sub_ <- paste(c(parts[-1], if (nzchar(sub_)) sub_), collapse = "")
  }
  if (tolower(base) %in% greek) base <- paste0("\\", tolower(base))
  if (nzchar(sub_)) paste0(base, "_{", sub_, "}") else base
}

# Markdown version (report.md)
bff_options_model_markdown <- function(opt, heading_level = 2) {
  info <- bff_options_model_info(opt)
  h <- function(level, txt) paste0(strrep("#", heading_level + level - 1), " ", txt)
  md <- c(h(1, "Fitted model"), "")
  if (!is.null(info$model_template)) md <- c(md, sprintf("Built from the model library: **%s**.", info$model_template), "")
  if (nzchar(info$system_latex)) {
    md <- c(md, "$$", "\\begin{aligned}", sprintf("\\frac{d%s}{dt} &= %s \\\\", info$vars, info$rhs_latex), "\\end{aligned}", "$$", "")
  }
  if (length(info$ic_text)) md <- c(md, paste0("**Initial conditions:** ", paste(info$ic_text, collapse = "; ")), "")
  if (length(info$fit_text)) md <- c(md, paste0("**Fitted quantities:** ", paste(info$fit_text, collapse = "; "), ". Error structure: ", info$err_text,
                                              if (length(info$err_priors)) paste0(" (priors of the error parameters - ", paste(info$err_priors, collapse = "; "), ")") else "", "."), "")
  if (NROW(info$param_table) > 0) {
    tb <- info$param_table
    tb$Parameter <- paste0("$", tb$Parameter, "$")
    md <- c(md, bff_md_table(tb), "")
  }
  if (length(info$composites) > 0) {
    md <- c(md, paste0("**Composite quantities:** ", paste(sprintf("%s = `%s`", names(info$composites), unlist(info$composites)), collapse = "; ")), "")
  }
  if (length(info$tdp_display) > 0) {
    md <- c(md, paste0("**Time-dependent parameters:** ", paste(unname(info$tdp_display), collapse = ", ")), "")
  }
  md
}
