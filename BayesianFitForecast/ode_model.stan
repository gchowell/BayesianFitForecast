functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta = theta[1];
    real i0 = theta[2];
    real rho = x_i[1];
    real N = x_i[2];
    real gamma = x_r[1];
    real kappa = x_r[2];
    real init[5] = {N-i0, 0, i0, 0, i0};

    real S = y[1]+init[1];
    real E = y[2]+init[2];
    real I = y[3]+init[3];
    real R = y[4]+init[4];
    real C = y[5]+init[5];

    real dS_dt = -beta * I * S / N;
    real dE_dt = beta * I * S / N - kappa * E;
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
    int rho;
    int N;
    real gamma;
    real kappa;
}
  
transformed data {
    array[2] real x_r = {gamma,kappa};
    array[2] int x_i = {rho,N};

  }parameters {
    real<lower=0> beta;
    real<lower=0, upper=100> i0;
    real<lower=0> sigma1;
}
transformed parameters {
  array[n_days + nfst_days, 5] real y;
  array[2] real theta;
  theta[1] = beta;
  theta[2] = i0;

  y = integrate_ode_rk45(ode, rep_array(0.0, 5), t0, ts, theta, x_r, x_i);
}
model {
  beta ~ uniform(0, 10);
  i0 ~ normal(0, 10);

  sigma1 ~ cauchy(0, 2.5)T[0,];

  for (t in 1:n_days) {
    cases1[t] ~ normal(fmax(1e-6, rho * kappa * y[t,2]), sigma1);
  }
}

generated quantities {
array[n_days + nfst_days] real pred_cases1;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = normal_rng(fmax(1e-6, rho * kappa * y[t,2]), sigma1);
  }

  real R0 = beta / gamma;
}

