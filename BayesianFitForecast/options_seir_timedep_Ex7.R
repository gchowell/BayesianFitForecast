# options.R

# Set the calibration period and forecasting horizon
calibrationperiods <- c(100)

# Set the calibration period and forecasting horizon
forecastinghorizon <- 0

model_name <- "SEIR-tdp"

# Define the state variable names
vars <- c("S","E","I","R","C")


params <- c("beta0","beta1", "q", "kappa", "gamma", "rho", "N", "t_int")

time_dependent_templates <- list(
  time_dependent_param1 = "if (t < params8) { return (params1); } else { return (params2 + (params1 - params2) * exp(-params3 * (t - params8))); }"
)


ode_system <- '
  diff_var1 = -time_dependent_param1 * vars1 * vars3 / params7
  diff_var2 = time_dependent_param1 * vars1 * vars3 / params7 - params4 * vars2
  diff_var3 = params4 * vars2 - params5 * vars3
  diff_var4 = params5 * vars3
  diff_var5 = params4 * params6 * vars2'

# Indicate if a parameter is fixed
paramsfix <- c(0,0,0,0,0,0,1,1)

# To generate interesting parameters
composite_expressions <- list(
  R0 = "beta0 / gamma"
)

# index of the model's variable that will be fit to the observed time series data
fitting_index <- c(5)

# boolean variable to indicate if the derivative of model's fitting variable should be fit to data.
fitting_diff <- c(1)

#Select the type of error structure 1.Negative binomial 2. Normal 3. Poisson
errstrc <- 2


#Define your input file
cadfilename1 <- "seir_simulated"

# string indicating the name of the disease related to the time series data
caddisease <- "simulated"

series_cases <- c("Cases")

datetype <- "Days"

# User-defined priors: For each parameter, define the prior distribution by 
# using _prior at the end. Since all parameters are positive values, it is 
# recommended to truncate the distribution at zero. Therefore, when using a normal
# distribution, use T[0,] to say that it is truncated at zero.

params1_prior <- "normal(0.5, 1)T[0,]"
params2_prior <- "normal(0.5, 1)T[0,]"
params3_prior <- "normal(0.5, 1)T[0,]"
params4_prior <- "normal(0.5, 1)T[0,]"
params5_prior <- "normal(0.5, 1)T[0,]"
params6_prior <- "uniform(0,1)"
params7_prior <- 1000000
params8_prior <- 10



# Define the lower bound of parameters
params1_LB <- 0
params2_LB <- 0
params3_LB <- 0
params4_LB <- 0
params5_LB <- 0
params6_LB <- 0
params7_LB <- 0
params8_LB <- 0



# Define the upper bound of parameters
params1_UB <- NA
params2_UB <- NA
params3_UB <- NA
params4_UB <- NA
params5_UB <- NA
params6_UB <- 1
params7_UB <- 10000000
params8_UB <- NA



# Select the prior distribution when using a normal or negative binomial 
# error structure
normalerror1_prior <- "cauchy(0, 2.5)"
negbinerror1_prior <- "exponential(5)"

# Select 0 if you want the initial condition be estimated as well. Otherwise, select 1
vars.init <- 1

# Enter the initial condition for variables
Ic = c(980000,0,20000,0,20000)

# number of MCMC steps
niter <- 100

# number of chains
num_chain = 2



