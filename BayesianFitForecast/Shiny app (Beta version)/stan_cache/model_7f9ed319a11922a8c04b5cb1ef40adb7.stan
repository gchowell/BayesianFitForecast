functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta = theta[1];
    real gamma = theta[2];
    real N = x_i[1];
    real S = y[1];
    real I = y[2];
    real R = y[3];
    real C = y[4];

    real dS_dt = - beta * S * I / N;
    real dI_dt = beta * S * I / N - gamma * I;
    real dR_dt = gamma * I;
    real dC_dt = beta * S * I / N; 
 return{dS_dt,dI_dt,dR_dt,dC_dt};
 }

}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;array[4] real y0;
real t0;
array[n_days + nfst_days] real ts;
array[n_days] int cases1;
    int N;
}
  
transformed data {
    array[0] real x_r;
    array[1] int x_i = {N};

  }parameters {
    real<lower=0> beta;
    real<lower=0> gamma;
    real<lower=0> phi_inv1;
}
transformed parameters {
  array[n_days + nfst_days, 4] real y;
  real phi1 = 1.0 / phi_inv1;
  array[2] real theta;
  theta[1] = beta;
  theta[2] = gamma;

  y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);
}
model {
  beta ~ normal(0.5, 10.0)T[0,];
  gamma ~ normal(0.25, 10.00)T[0,];

  phi_inv1 ~ exponential(5);

  for (t in 1:n_days) {
    cases1[t] ~ neg_binomial_2(fmax(1e-6, beta * y[t,1] * y[t,2] / N), phi1);
  }
}

generated quantities {
  array[n_days + nfst_days] real mu_cases1;
  array[n_days + nfst_days] real pred_cases1;
  array[n_days] real log_lik1;
  for (t in 1:(n_days + nfst_days)) {
    mu_cases1[t] = fmax(1e-6, beta * y[t,1] * y[t,2] / N);
    pred_cases1[t] = neg_binomial_2_rng(mu_cases1[t], phi1);
  }
  for (t in 1:n_days) {
    log_lik1[t] = neg_binomial_2_lpmf(cases1[t] | mu_cases1[t], phi1);
  }
}

