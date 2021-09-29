
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:56:23 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Interpolate Climate rasters
#################################

library(raster)
library(stringr)
library(rgdal)


library(akima)
library(raster)
library(rgdal)
library(stringr)

rasterOptions(chunksize=2e+10, progress='text')

rootDir <- paste0('d:/Projects/Climate/BAWAP/Summaries/SummaryRasters')
templateR <- raster('C:/Projects/Roper/Boundaries/demcrop.tif')
outDir <- 'C:/Temp/Roper'

fls <- list.files(rootDir, full.names = T,  '*.tif$')
fls <- fls[c(1,7,8)]

for (i in 1:length(fls)){
      print(fls[i])
      inR <- raster(fls[i])
      cc  <- crop(inR, templateR, snap='out' )
      pts <- rasterToPoints(cc)
      pts2 <- as.data.frame(pts)
      s = interp(x=pts2$x, y=pts2$y, z=pts2[,3], nx=ncol(templateR), ny=nrow(templateR), linear = T)
      
      r2 <- raster(templateR)
      r2[] <- t(s$z[])
      r3 <- flip(r2 , 2)
      plot(r3)
      writeRaster(r3, paste0(outDir, '/', basename(fls[i])), overwrite = T)
}


rt <- raster('C:/Projects/Roper/Suitability/LimitationRasters/dem1sv1_0_Roper.tif')
r3 <- raster('C:/Temp/Roper/tmax_DryGreaterThan38_Avg.tif')
compareRaster(rt, r3)


head(pts2)
coordinates(pts2) <- ~x+y
plot(r3)
points(pts2, pch=1)
r2 <- raster('C:/Projects/Roper/Covariates/finessed/Clim_minann.tif')
compareRaster(r3, r2)

