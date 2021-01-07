## Reference : 
## https://rstudio-pubs-static.s3.amazonaws.com/243277_01730c1f0a984132bce5d5d25bec62aa.html

library(spdep)
library(rstan)
library(ggplot2)
library(tmap)

setwd("./SS/SpatialStat20/modelling/rstan_spdep_spmodeling")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('ireland.RData')
ireland.nb <- poly2nb(ireland, snap = 0.0002)

plot(ireland)
plot(ireland.nb, coords = coordinates(ireland), add = TRUE, pch = 16, col = "darkred")


## 1.1 
tm_shape(ireland) + tm_polygons(col = "A")
tm_shape(ireland) + tm_polygons(col='POPCHG',title='1961 Popn as % of 1926 Popn')

# linear trend # A ~ POPCHG
ggplot(data = data.frame(ireland), aes(x = POPCHG, y = A)) +
  geom_point() + geom_smooth(method = 'lm')

library(MASS) # using a robust approach `rlm` in `MASS`


## 1.3 spatial outliers
ireland$resids <- rlm(A~POPCHG, data = data.frame(ireland))$res
tm_shape(ireland) + tm_polygons(col = 'resids', title = 'Residuals') + tm_style_col_blind()

# correlation between two variables
# some ourliers detected
# the error term has some degree of spatial autocorrelation

moran.test(ireland$A,nb2listw(ireland.nb))


# spdep # `errorsarlm` function
library(spatialreg)
sp_model <- errorsarlm(A~POPCHG, data= data.frame(ireland), listw = nb2listw(ireland.nb))

summary(sp_model)
ols_model <- lm(A~POPCHG, data= data.frame(ireland))
cbind(coef(ols_model), coef(sp_model)[-1]) 
# beta reduces, no longer significantly different from zero


## 2.1 Aside : more comprehensible coefficients
# standardise the predictor variables x, but not Y
# coefficients for each of the predictors will have the same units, and be comparable, relative influence.

rescale <- function(x) (x-mean(x))/(quantile(x, 0.975)-quantile(x, 0.025))
sp_model_scaled <- errorsarlm(A ~ rescale(POPCHG), data = data.frame(ireland), listw = nb2listw(ireland.nb))
summary(sp_model_scaled)


# 3. A Bayesian approach
# SAR model
N <- nrow(ireland)
I <- diag(N)
W <- nb2mat(ireland.nb)
e = rep(1,N)
data_car <- list(N = nrow(ireland), x = ireland$POPCHG, 
                 y = ireland$A, W = W, I = I)
car <- stan(file = 'car.stan', data = data_car)

car_df <- as.data.frame(car)
ggplot(car_df, aes(x = lambda, y = beta)) + 
  geom_point(alpha = 0.4, col = 'indianred') +
  geom_density2d(col = 'darkblue')

car_robust <- stan(file = 'car_robust.stan', 
                   data = data_car)
car_robust_df <- as.data.frame(car_robust)
ggplot(car_ht_df,aes(x=nu,y=lambda)) +  geom_point(alpha=0.4,col='indianred') + geom_density2d(col='darkblue') + geom_smooth(method='loess')


## 3.3 SVC
data_svc <- list(N = nrow(ireland), x = ireland$POPCHG, 
                 y = ireland$A, W = W, e=e, I = I)
svc <- stan(file = 'SVC.stan', data = data_svc, iter = 5000)

beta_sims <- as.matrix(svc, pars = 'beta')
ireland$local_beta <- apply(beta_sims, 2, mean)
tm_shape(ireland) + tm_polygons(col = 'local_beta', title = 'Local beta')

ireland$local_beta_sd <- apply(beta_sims,2,sd)
tm_shape(ireland) + tm_polygons(col='local_beta_sd',title='Beta SD')

svc_df <- as.data.frame(svc)
ggplot(svc_df,aes(x=lambda_b,y=lambda)) +  geom_point(alpha=0.4,col='indianred') + geom_density2d(col='darkblue') 

cor(svc_df$lambda, svc_df$lambda_b)
