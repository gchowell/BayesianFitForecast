functions {

real time_dependent_param1(real t, real t_int, real beta0, real beta1, real q) {
if (t < t_int) { return (beta0); } else { return (beta1 + (beta0 - beta1) * exp(-q * (t - t_int))); }
}


    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta0 = theta[1];
    real beta1 = theta[2];
    real q = theta[3];
    real kappa = theta[4];
    real gamma = theta[5];
    real rho = theta[6];
    real N = x_i[1];
    real t_int = x_i[2];
    real S = y[1];
    real E = y[2];
    real I = y[3];
    real R = y[4];
    real C = y[5];

    real dS_dt = -time_dependent_param1(t, t_int, beta0, beta1, q) * S * I / N;
    real dE_dt = time_dependent_param1(t, t_int, beta0, beta1, q) * S * I / N - kappa * E;
    real dI_dt = kappa * E - gamma * I;
    real dR_dt = gamma * I;
    real dC_dt = kappa * rho * E; 
 return{dS_dt,dE_dt,dI_dt,dR_dt,dC_dt};
 }

}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;array[5] real y0;
real t0;
array[n_days + nfst_days] real ts;
array[n_days] int cases1;
    int N;
    int t_int;
}
  
transformed data {
    array[0] real x_r;
    array[2] int x_i = {N,t_int};

  }parameters {
    real<lower=0> beta0;
    real<lower=0> beta1;
    real<lower=0> q;
    real<lower=0> kappa;
    real<lower=0> gamma;
    real<lower=0, upper=1> rho;
    real<lower=0> sigma1;
}
transformed parameters {
  array[n_days + nfst_days, 5] real y;
  array[6] real theta;
  theta[1] = beta0;
  theta[2] = beta1;
  theta[3] = q;
  theta[4] = kappa;
  theta[5] = gamma;
  theta[6] = rho;

  y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);
}
model {
  beta0 ~ normal(0.5, 1)T[0,];
  beta1 ~ normal(0.5, 1)T[0,];
  q ~ normal(0.5, 1)T[0,];
  kappa ~ normal(0.5, 1)T[0,];
  gamma ~ normal(0.5, 1)T[0,];
  rho ~ uniform(0,1);

  sigma1 ~ cauchy(0, 2.5);

  for (t in 1:n_days) {
    cases1[t] ~ normal(fmax(1e-6, kappa * rho * y[t,2]), sigma1);
  }
}

generated quantities {
array[n_days + nfst_days] real pred_cases1;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = normal_rng(fmax(1e-6, kappa * rho * y[t,2]), sigma1);
  }

  real R0 = beta0 / gamma;
}

