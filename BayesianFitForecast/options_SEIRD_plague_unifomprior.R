# options.R

# Set the calibration period and forecasting horizon
calibrationperiods <- c(10,12,14,16,18,20,22,24,26,28)

# Set the calibration period and forecasting horizon
forecastinghorizon <- 6

model_name <- "Bayesian-noninformative"

# Define the state variable names
vars <- c("S", "E", "I", "R", "D")

# Define the parameters
# Example:
# beta: the transmission rate. the range for the Bombay plague is (0,10)
# gamma: the recovery/death rate. the range for the Bombay plague is (0,10)
# kappa: the incubation rate. the range for the Bombay plague is (0,10)
# rho: the death proportion rate. the range is (0,1)
# N: The population size. It is a fixed number for this case study: 100,000
params <- c("beta", "gamma", "kappa", "rho","N")

ode_system <- '
  diff_var1 = -params1 * vars3 * vars1 / params5
  diff_var2 = params1 * vars3 * vars1 / params5 - params3 * vars2
  diff_var3 = params3 * vars2 - params2 * vars3
  diff_var4 = params2 * (1-params4) * vars3
  diff_var5 = params4 * params2 * vars3'


# Indicate if a parameter is fixed
paramsfix <- c(0,0,0,0,1)

# To generate interesting parameters
composite_expressions <- list(
  R0 = "beta / gamma",
  recovery_time = "1 / gamma"
)

# index of the model's variable that will be fit to the observed time series data
fitting_index <- c(5)

# boolean variable to indicate if the derivative of model's fitting variable should be fit to data.
fitting_diff <- c(1)

#Select the type of error structure 1.Negative binomial 2. Normal 3. Poisson
errstrc <- 2


#Define your input file
cadfilename1 <- "curve-plague-bombay"

# string indicating the name of the disease related to the time series data
caddisease <- "plague"

series_cases <- c("Deaths")

datetype <- "weeks"

# User-defined priors: For each parameter, define the prior distribution by 
# using _prior at the end. Since all parameters are positive values, it is 
# recommended to truncate the distribution at zero. Therefore, when using a normal
# distribution, use T[0,] to say that it is truncated at zero.

params1_prior <- "uniform(0, 10)"
params2_prior <- "uniform(0, 10)"
params3_prior <- "uniform(0, 10)"
params4_prior <- "uniform(0, 1)"
params5_prior <- 100000

# Define the lower bound of parameters
params1_LB <- 0
params2_LB <- 0
params3_LB <- 0
params4_LB <- 0

# Define the upper bound of parameters
params1_UB <- NA
params2_UB <- NA
params3_UB <- NA
params4_UB <- 1
# Select the prior distribution when using a normal or negative binomial 
# error structure
normalerror1_prior <- "cauchy(0, 2.5)"
negbinerror1_prior <- "exponential(5)"

# Select 0 if you want the initial condition be estimated as well. Otherwise, select 1
vars.init <- 1

# Enter the initial condition for variables
Ic = c(99992,0,8,0,8)

# number of MCMC steps
niter <- 10000

# number of chains
num_chain = 2


