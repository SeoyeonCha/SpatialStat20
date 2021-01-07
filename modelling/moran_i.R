load(url("http://github.com/mgimond/Spatial/raw/master/Data/moransI.RData"))


### Moran's I 

# 1. Define neighbor
# 2. Construct neighborhood matrix(nb2listw, style = "W", "B")
# 3. moran.test(data, neighborhood matrix)
# 3. lagged value ~ value lm fitting -> Moran's I statistic
# 4. MC simulation

s1
library(tmap)

tm_shape(s1) + tm_polygons(style = "quantile", col = "Income") + 
  tm_legend(outside = TRUE)


## Define neighboring polygons. 
library(spdep)
nb <- poly2nb(s1, queen = TRUE)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)


Inc.lag <- lag.listw(lw, s1$Income)

mean(s1[[2]][c(2,3,4,5)])



M <- lm(Inc.lag ~ s1$Income)
plot(Inc.lag ~ s1$Income, pch = 20, asp = 1, las = 1)
coef(M)[2]

n <- 599L
I.r <- vector(length = n)
for(i in 1:n){
  x <- sample(s1$Income, replace = FALSE)
  x.lag <- lag.listw(lw, x)
  M.r <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

hist(I.r, las =1, main = NULL)
abline(v = coef(M)[2], col = 2)
1-sum(coef(M)[2] > I.r)/n


moran.test(s1$Income, lw)
MC <- moran.mc(s1$Income, lw, nsim = 599)

plot(MC, las = 1)

coo <- coordinates(s1)
S.dist <- dnearneigh(coo, 0, 50000)
lw <- nb2listw(S.dist, style = "W", zero.policy = T)
MI <- moran.mc(s1$Income, lw, nsim = 599, zero.policy = TRUE)
plot(MI)

MI
