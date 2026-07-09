functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta = theta[1];
    real rho = x_i[1];
    real N = x_i[2];
    real gamma = x_r[1];
    real kappa = x_r[2];
    real S = y[1];
    real E = y[2];
    real I = y[3];
    real R = y[4];
    real C = y[5];

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
    int<lower=0> nfst_days;array[5] real y0;
real t0;
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
}
transformed parameters {
  array[n_days + nfst_days, 5] real y;
  {
    array[1] real theta;
    theta[1] = beta;
    y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);
  }
}
model {
  beta ~ uniform(0, 10);

  for (t in 1:n_days) {
    cases1[t] ~ poisson(fmax(1e-6, rho * kappa * y[t,2]));
  }
}

generated quantities {
  array[n_days + nfst_days] real pred_cases1;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = poisson_rng(fmax(1e-6, rho * kappa * y[t,2]));
  }

  real R0 = beta / gamma;
}

