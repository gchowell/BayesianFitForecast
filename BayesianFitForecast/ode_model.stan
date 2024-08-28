functions {
  real[] ode(real t, real[] y, real[] theta, real[] x_r, int[] x_i) { 

 
    real r = theta[1];
    real N = x_i[1];
    real C = y[1];

    real dC_dt = r * C; 
 return{dC_dt};
 }
}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;real y0[1];real t0;
    real ts[n_days + nfst_days];
    int cases[n_days];
    int N;
}
  
transformed data {
    real x_r[0];
    int x_i[1] = {N};

  }
parameters {
    real<lower=0> r;
    real<lower=0> phi_inv;
}
    
transformed parameters {
    real y[n_days + nfst_days, 1];
    real phi = 1. / phi_inv;

    {
    real theta[1];
        theta[1] = r;
y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);
 }
}
    
model {
      r ~ uniform(0, 10);
  phi_inv ~exponential(5);
  for (t in 1:n_days) {
    cases[t] ~ neg_binomial_2(fmax(1e-6,r * y[t,1]), phi);
  }
}
    
generated quantities {
    real pred_cases[n_days + nfst_days];
    for (t in 1:n_days + nfst_days) {
        pred_cases[t] = neg_binomial_2_rng(fmax(1e-6,r * y[t,1]), phi);
    }
}
