// spatial and robust models
// using multivariate t distn

data{
  int N;
  vector[N] x;
  vector[N] y;
  matrix<lower=0>[N,N] W;
  matrix<lower=0,upper=1>[N,N] I;
}

parameters{
  real beta;
  real<lower=0,upper=100> alpha;
  real<lower=0> sigma;
  real<lower=0> nu;
  real<lower=-1,upper=1> lambda;
}

model{
  nu~gamma(2,0.1);
  y~multi_student_t(nu, alpha + x*beta, tcrossprod(inverse(I-lambda * W))*sigma*sigma);
}
