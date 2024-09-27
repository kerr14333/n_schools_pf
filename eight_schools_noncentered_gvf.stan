data {
  int <lower=0> J; // number of schools
  array[J] real y; // estimated treatment
  array[J] real<lower=0> sigma; // std of estimated effect
}
transformed data{
  array[J] real<lower=0> sigma_2;
  sigma_2 = pow(sigma,2);
}
parameters {
  vector[J] theta_trans; // transformation of theta
  real mu; // hyper-parameter of mean
  real<lower=0> tau; // hyper-parameter of sd
  real<lower=0> phi;
}
transformed parameters{
  vector[J] theta;   
  real<lower=0> sqrt_phi;
  real<lower=0> tau_sq;
  // original theta
  theta=theta_trans*tau+mu;
  sqrt_phi = sqrt(phi);
  tau_sq = pow(tau,2);
}
model {
  theta_trans ~ normal (0,1);
  y ~ normal(theta , sqrt_phi);
  sigma_2 ~ inv_gamma(2, phi ); 
  mu ~ normal(0, 5); // a non-informative prior
  tau ~ cauchy(0, 5);
  phi ~ gamma(1,1);
}
