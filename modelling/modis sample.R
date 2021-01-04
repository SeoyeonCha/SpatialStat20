#install.packages("MODISTools")

# load the library
library(MODISTools)

# download data
subset <- mt_subset(product = "MOD11A2",
                    lat = 40,
                    lon = -110,
                    band = "LST_Day_1km",
                    start = "2004-01-01",
                    end = "2004-02-01",
                    km_lr = 1,
                    km_ab = 1,
                    site_name = "testsite",
                    internal = TRUE,
                    progress = FALSE)
print(str(subset))

# create data frame with a site_name, lat and lon column
# holding the respective names of sites and their location
df <- data.frame("site_name" = paste("test",1:2))
df$lat <- 40
df$lon <- -110

# test batch download
subsets <- mt_batch_subset(df = df,
                           product = "MOD11A2",
                           band = "LST_Day_1km",
                           internal = TRUE,
                           start = "2004-01-01",
                           end = "2004-02-01")

print(str(subsets))

products <- mt_products()
head(products)

bands <- mt_bands(product = "MOD11A2")
head(bands)



mt_to_raster(subset)

library(raster) ; library(sp) ; library(MODIS)
loc <- c(37.27170295537566, 126.98836171933276)
loc
