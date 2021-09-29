#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:30:00 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Mask and reproject covariate rasters
#################################

library(raster)
library(stringr)
library(rgdal)

library(raster)

outDir1 <- 'e:/Projects/Roper/Covariates/Masked'
outDir2 <- 'e:/Projects/Roper/Covariates/UTM'
inDir <-  'e:/Projects/Roper/Covariates/finessed'

catMaskR <- raster('C:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')

fls <- list.files(inDir, full.names = T)

for (i in 1:length(fls)) {
  
  r <- raster(fls[i])
  rn <- basename(fls[i])
  r2 <- mask(r, catMaskR, filename = paste0(outDir1, '/', rn), overwrite=T)
}


crsTemplate <- readOGR('c:/projects/Roper/boundaries/att2053.shp')
rCRS <- crs(crsTemplate)

fls <- list.files(outDir1, full.names = T)

for (i in 1:length(fls)) {
  
  r <- raster(fls[i])
  rn <- basename(fls[i])
  r2 <- projectRaster(r,crs=rCRS, method='bilinear', filename = paste0(outDir2, '/', rn), overwrite=T)

}