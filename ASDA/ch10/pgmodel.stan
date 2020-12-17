data{
  int N; 
  int observed[N];
  real<lower = 0> expected[N];
}

parameters{
  real<lower = 0> nu;
  real<lower = 0> alpha;
  real<lower = 0> theta[N];
}

transformed parameters{
  real<lower = 0> mu[N];
  for (n in 1:N)
    mu[n] = expected[n] * theta[n];
}

model{
  for (n in 1:N){
    observed[n] ~ poisson(mu[n]);
  }
  nu ~ gamma(0.01, 0.01);
  alpha ~ gamma(0.01, 0.01);
  theta ~ gamma(nu, alpha);
}
