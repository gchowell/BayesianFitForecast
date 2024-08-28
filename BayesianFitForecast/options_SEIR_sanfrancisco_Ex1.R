# options.R

# Set the calibration period and forecasting horizon
calibrationperiods <- c(17)

# Set the calibration period and forecasting horizon
forecastinghorizon <- 0

model_name <- "Bayesian-uniform"

# Define the state variable names
vars <- c("S", "E", "I", "R", "C")

# Define the parameters
# Example:
# beta: the transmission rate. the range for the flu 1918 is (0,2)
# gamma: the recovery rate. the range for the flu 1918 is (0,1)
# kappa: the incubation rate. the range for the flu 1918 is (0,2)
# rho: the recovery proportion rate. the range for the flu 1918 is (0,1)
# N: The population size. It is a fixed number for this case study: 550,000
params <- c("beta", "gamma", "kappa", "rho","N")

ode_system <- '
  diff_var1 = -params1 * vars3 * vars1 / params5
  diff_var2 = params1 * vars3 * vars1 / params5 - params3 * vars2
  diff_var3 = params3 * vars2 - params2 * vars3
  diff_var4 = params2 * vars3
  diff_var5 = params4 * params3 * vars2'

# Indicate if a parameter is fixed
paramsfix <- c(0,1,1,1,1)

# To generate interesting parameters
composite_expressions <- list(
  R0 = "beta / gamma",
  recovery_time = "1 / gamma"
)

# index of the model's variable that will be fit to the observed time series data
fitting_index <- 5

# boolean variable to indicate if the derivative of model's fitting variable should be fit to data.
fitting_diff <- 1

#Select the type of error structure 1.Negative binomial 2. Normal 3. Poisson
errstrc <- 3


#Define your input file
cadfilename1 <- "SanFrancisco"

# string indicating the name of the disease related to the time series data
caddisease <- "sanfrancisco"

datatype <- "Cases"

datetype <- "Days"

# User-defined priors: For each parameter, define the prior distribution by 
# using _prior at the end. Since all parameters are positive values, it is 
# recommended to truncate the distribution at zero. Therefore, when using a normal
# distribution, use T[0,] to say that it is truncated at zero.

params1_prior <- "uniform(0, 10)"
params2_prior <- 1/4.1
params3_prior <- 1/1.9
params4_prior <- 1
params5_prior <- 550000

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
normalerror_prior <- "cauchy(0, 2.5)"
negbinerror_prior <- "exponential(5)"

# Select 0 if you want the initial condition be estimated as well. Otherwise, select 1
vars.init <- 1

# Enter the initial condition for variables
Ic = c(549996,0,4,0,4)

# number of MCMC steps
niter <- 1000

# number of chains
num_chain = 2



