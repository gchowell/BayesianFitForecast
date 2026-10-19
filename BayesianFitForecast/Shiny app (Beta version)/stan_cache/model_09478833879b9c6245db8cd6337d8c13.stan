functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta = theta[1];
    real N = x_i[1];
    real i0 = x_i[2];
    real rho = x_i[3];
    real kappa = x_r[1];
    real gamma = x_r[2];
    array[5] real init = {N-i0, 0, i0, 0, i0};

    real S = y[1]+init[1];
    real E = y[2]+init[2];
    real I = y[3]+init[3];
    real R = y[4]+init[4];
    real C = y[5]+init[5];

    real dS_dt = - beta * S * I / N;
    real dE_dt = beta * S * I / N - kappa * E;
    real dI_dt = kappa * E - gamma * I;
    real dR_dt = gamma * I;
    real dC_dt = rho * kappa * E; 
 return{dS_dt,dE_dt,dI_dt,dR_dt,dC_dt};
 }

}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;real t0;
array[n_days + nfst_days] real ts;
array[n_days] int cases1;
    int N;
    int i0;
    int rho;
    real kappa;
    real gamma;
}
  
transformed data {
    array[2] real x_r = {kappa,gamma};
    array[3] int x_i = {N,i0,rho};

  }parameters {
    real<lower=0> beta;
    real<lower=0> phi_inv1;
}
transformed parameters {
  array[n_days + nfst_days, 5] real y;
  array[5] real init_ic = {N-i0, 0, i0, 0, i0};
  real phi1 = 1.0 / phi_inv1;
  array[1] real theta;
  theta[1] = beta;

  y = integrate_ode_rk45(ode, rep_array(0.0, 5), t0, ts, theta, x_r, x_i);
}
model {
  beta ~ normal(0.5, 1.0)T[0,];

  phi_inv1 ~ exponential(5);

  for (t in 1:n_days) {
    cases1[t] ~ neg_binomial_2(fmax(1e-6, rho * kappa * (y[t,2] + init_ic[2])), phi1);
  }
}

generated quantities {
  array[n_days + nfst_days] real mu_cases1;
  array[n_days + nfst_days] real pred_cases1;
  array[n_days] real log_lik1;
  for (t in 1:(n_days + nfst_days)) {
    mu_cases1[t] = fmax(1e-6, rho * kappa * (y[t,2] + init_ic[2]));
    pred_cases1[t] = neg_binomial_2_rng(mu_cases1[t], phi1);
  }
  for (t in 1:n_days) {
    log_lik1[t] = neg_binomial_2_lpmf(cases1[t] | mu_cases1[t], phi1);
  }

  real R0 = beta / gamma;
}

