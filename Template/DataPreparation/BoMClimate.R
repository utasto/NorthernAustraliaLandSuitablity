
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 13:09:38 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Resample the BoM rasters to remove the 5km pixel effects in the output products
#################################

library(raster)
library(stringr)
library(rgdal)

library(akima)
library(raster)
library(rgdal)
library(stringr)

templateR <- raster('E:/Roper/DataMassage/DEM/demcrop.tif')
outDir <- 'C:/Temp/BoMClim'

Top <- -12.894
Bottom <- -16.778
Left <- 131.995
Right <- 135.7075
rext <- extent(Left, Right, Bottom, Top ) 
rext


# Resample the BoM rasters to remove the 5km pixel effects in the output products
# Original data sourced from http://www.bom.gov.au/climate/averages/maps.shtml
rootDir <- 'D:/TERN/Data/ClimateFromBOM/BOM/unzipped'
fls <- list.files(rootDir, full.names = T,  '*.txt')

for (i in 1:length(fls)){
      print(fls[i])
      inR <- raster(fls[i])
      cc  <- crop(inR, rext, snap='out' )
      pts <- rasterToPoints(cc)
      pts2 <- as.data.frame(pts)

      s = interp(x=pts2$x, y=pts2$y, z=pts2[,3], nx=ncol(templateR), ny=nrow(templateR), linear = T)
      
      r2 <- raster(templateR)
      r2[] <- t(s$z[])
      r3 <- flip(r2 , 2)
      plot(r3)
      writeRaster(r3, paste0(outDir, '/', str_replace(basename(fls[i]), '.txt', '.tif' )), overwrite = T)
}


sumr <- raster('C:/Temp/BoMClim/rainsum.tif')
plot(sumr)
winr <- raster('C:/Temp/BoMClim/rainwin.tif')
plot(winr)
ratio <- sumr/winr
rc <- clamp(ratio, upper=1000)
plot(rc)
plot(ratio)
writeRaster(rc,'C:/Temp/BoMClim/sum_wint_ratio.tif', overwrite=T)
rc
