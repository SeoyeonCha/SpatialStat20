data {
  int N;
  vector[N] x;
  vector[N] y;
  matrix<lower=0>[N,N] W;
  vector<upper=1>[N] e;
  matrix<lower=0,upper=1>[N,N] I;
}

parameters{
  vector[N] beta; // spatially varying coefs
  real<lower=0,upper=100> alpha;
  real mbeta;
  real<lower=0> sigma;
  real<lower=0> sigma_b;
  real<lower=-1,upper=1> lambda;
  real<lower=-1,upper=1> lambda_b;
}

model{
  beta ~ multi_normal_prec(e*mbeta, tcrossprod(I-lambda_b * W)/(sigma_b*sigma_b));
  y ~ multi_normal_prec(alpha + x .* beta, tcrossprod(I - lambda * W)/(sigma*sigma));
}
