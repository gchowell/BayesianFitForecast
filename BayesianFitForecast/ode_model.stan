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
    int<lower=0> nfst_days;real y0[5];real t0;
real ts[n_days + nfst_days];
int cases1[n_days];
    int rho;
    int N;
    real gamma;
    real kappa;
}
  
transformed data {
    real x_r[2] = {gamma,kappa};
    int x_i[2] = {rho,N};

  }
parameters {
    real<lower=0> beta;

 }
    
transformed parameters {
    real y[n_days + nfst_days, 5];
    {
    real theta[1];
        theta[1] = beta;
y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);
 }
}
    
model {
      beta ~ uniform(0, 10);

  for (t in 1:n_days) {    cases1[t] ~ poisson(fmax(1e-6, rho * kappa * y[t,2]));
}
}
    
generated quantities {    real pred_cases1[n_days + nfst_days];
    for (t in 1:n_days + nfst_days) {
        pred_cases1[t] = poisson_rng(fmax(1e-6, rho * kappa * y[t,2]));
    }

    // Composite quantities
        real R0 = beta / gamma;
}
