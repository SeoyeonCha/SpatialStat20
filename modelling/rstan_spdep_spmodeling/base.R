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
I <- diag(N)
W <- nb2mat(ireland.nb)
car <- stan_model(file = 'car.stan')
