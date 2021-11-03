
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:27:36 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Make the project template raster
#################################

library(raster)
library(stringr)
library(rgdal)

library(raster)
library(rgdal)

p <- readOGR('C:/Projects/Roper/Boundaries', 'river_mask')
r <- raster('C:/Projects/Roper/Boundaries/demcrop.tif')


allrivemask <- mask(r, p, inverse=T)
plot(allrivemask)
writeRaster(allrivemask, 'c:/temp/m1.tif')


r2 <- raster('C:/Projects/Roper/Covariates/all/Clim_minann.tif')
compareRaster(r,r2)


r1s <- allrivemask/allrivemask
plot(r1s)

catr <- raster('C:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')
plot(catr)

r2s <- catr/catr
r3s <- r2s
r3s[is.na(r3s)] <- 3
plot(r3s)

rc <- r1s + r3s
plot(rc)

r3s[r3s==1] <- 1
plot(r3s)

r4 <- r3s + r1s
plot(r4)
r4[r4==4] <- 1
plot(r4)

writeRaster(r4, 'C:/Projects/Roper/Boundaries/bdyBoxWithNaRiver.tif', datatype='INT1U')
