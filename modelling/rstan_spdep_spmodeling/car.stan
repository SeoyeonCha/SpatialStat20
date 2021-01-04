data {
   int N;
   vector[N] x;
   vector[N] y;
   matrix<lower=0>[N,N] W;
   matrix<lower=0,upper=1>[N,N] I;
}

parameters {
   real beta;
   real<lower=0,upper=100> alpha;
   real<lower = 0> sigma;
   real<lower=-1,upper=1> lambda;
}

model {
   y ~ multi_normal_prec(alpha + x * beta,  crossprod(I - lambda * W)/(sigma*sigma));
}

