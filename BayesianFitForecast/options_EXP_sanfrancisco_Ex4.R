# options.R

# Set the calibration period and forecasting horizon
calibrationperiods <- c(17)

# Set the calibration period and forecasting horizon
forecastinghorizon <- 10

model_name <- "Bayesian-exp"

# Define the state variable names
vars <- c("C")

# Define the parameters
# Example:
# r: the exponentional rate
# N: The population size. It is a fixed number for this case study: 550,000
params <- c("r","N")

ode_system <- '
  diff_var1 = params1 * vars1'

# Indicate if a parameter is fixed
paramsfix <- c(0,1)

# To generate interesting parameters
composite_expressions <- list(
  
)

# index of the model's variable that will be fit to the observed time series data
fitting_index <- 1

# boolean variable to indicate if the derivative of model's fitting variable should be fit to data.
fitting_diff <- 1

#Select the type of error structure 1.Negative binomial 2. Normal 3. Poisson
errstrc <- 1


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
params2_prior <- 550000

# Define the lower bound of parameters
params1_LB <- 0

# Define the upper bound of parameters
params1_UB <- NA

# Select the prior distribution when using a normal or negative binomial 
# error structure
normalerror_prior <- "cauchy(0, 2.5)"
negbinerror_prior <- "exponential(5)"

# Select 0 if you want the initial condition be estimated as well. Otherwise, select 1
vars.init <- 1

# Enter the initial condition for variables
Ic = c(4)

# number of MCMC steps
niter <- 1000

# number of chains
num_chain = 2



