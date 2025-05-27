# options.R

# Set the calibration period and forecasting horizon
calibrationperiods <- c(65)

# Set the calibration period and forecasting horizon
forecastinghorizon <- 20

model_name <- "Bayesian-switzerland"

# Define the state variable names
vars <- c("S", "E", "I", "U", "R", "H", "C")


params <- c("beta0","beta1", "q", "alpha", "k", "gamma", "rho", "rhoh","N","t_int")

time_dependent_templates <- list(
  time_dependent_param1 = "if (t < params10) { return (params1); } else { return (params2 + (params1 - params2) * exp(-params3 * (t - params10))); }"
)

ode_system <- '
  diff_var1 = -time_dependent_param1 * vars1 * pow(vars3 + vars4,params4) / params9
  diff_var2 = time_dependent_param1 * vars1 * pow(vars3 + vars4,params4) / params9 - params5 * vars2
  diff_var3 = params5 * params7 * vars2 - params6 * vars3
  diff_var4 = params5 * (1-params7) * vars2 - params6 * vars4
  diff_var5 = params6 * (1-params8) * vars3 + params6 * vars4
  diff_var6 = params6 * params8 * vars3
  diff_var7 = params5 * params7 * vars2'

# Indicate if a parameter is fixed
paramsfix <- c(0,0,0,1,1,1,0,0,1,1)

# To generate interesting parameters
composite_expressions <- list(
  R0 = "beta0 / gamma"
)

# index of the model's variable that will be fit to the observed time series data
fitting_index <- c(6,7)

# boolean variable to indicate if the derivative of model's fitting variable should be fit to data.
fitting_diff <- c(1,1)

#Select the type of error structure 1.Negative binomial 2. Normal 3. Poisson
errstrc <- 2


#Define your input file
cadfilename1 <- "MyData"

# string indicating the name of the disease related to the time series data
caddisease <- "Switzerland"

series_cases <- c("Hospitalized", "Cases")

datetype <- "Days"

# User-defined priors: For each parameter, define the prior distribution by 
# using _prior at the end. Since all parameters are positive values, it is 
# recommended to truncate the distribution at zero. Therefore, when using a normal
# distribution, use T[0,] to say that it is truncated at zero.

params1_prior <- "normal(0.5, 1)T[0,]"
params2_prior <- "normal(0.5, 1)T[0,]"
params3_prior <- "normal(0.5, 1)T[0,]"
params4_prior <- 1
params5_prior <- 0.2
params6_prior <- 0.25
params7_prior <- "uniform(0,1)"
params8_prior <- "uniform(0,1)"
params9_prior <- 8522630
params10_prior <- 47

# Define the lower bound of parameters
params1_LB <- 0
params2_LB <- 0
params3_LB <- 0
params4_LB <- NA
params5_LB <- NA
params6_LB <- NA
params7_LB <- 0
params8_LB <- 0
params9_LB <- NA
params10_LB <- NA


# Define the upper bound of parameters
params1_UB <- NA
params2_UB <- NA
params3_UB <- NA
params4_UB <- NA
params5_UB <- NA
params6_UB <- NA
params7_UB <- 1
params8_UB <- 1
params9_UB <- NA
params10_UB <- NA


# Select the prior distribution when using a normal or negative binomial 
# error structure
normalerror1_prior <- "cauchy(0, 2.5)"
normalerror2_prior <- "cauchy(0, 2.5)"
negbinerror1_prior <- "exponential(5)"

# Select 0 if you want the initial condition be estimated as well. Otherwise, select 1
vars.init <- 1

# Enter the initial condition for variables
Ic = c(8522629,0,1,0,0,1,1)

# number of MCMC steps
niter <- 2000

# number of chains
num_chain = 2



