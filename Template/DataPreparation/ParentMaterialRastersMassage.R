
#################################
###  Author : Ross Searle         
###  Date : Tue Jan 14 11:59:04 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : massage the Parent Material covariate rasters
###  Notes : 
#################################

library(raster)
library(stringr)
library(rgdal)

rootDir <- '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/Parent_Material'
outRoot <-  'E:/Roper/DataMassage/PM'
fls <- list.files(rootDir, '*.tif$', full.names = T)

for(a in 1:length(fls)){
  print(fls[a])
  outPath <- paste0( outRoot, '/crp/', basename(fls[a]))
  if(!file.exists(outPath)){
    mos <- raster(fls[a])
    c1  <- crop(mos, rext, snap='out', filename=outPath)
    # rr <- resample(c1, templateR, filename=  , methods[a])
  }
}


outRoot <-  'E:/Roper/DataMassage/PM'
fls <- list.files(paste0(outRoot, '/crp'), '*.tif$', full.names = T)
templateR <- raster('E:/Roper/DataMassage/DEM/demcrop.tif')

for(a in 1:length(fls)){
  print(fls[a])
  outPath <- paste0( outRoot, '/', basename(fls[a]))
  if(!file.exists(outPath)){
    mos <- raster(fls[a])
    #c1  <- crop(mos, rext, snap='out', filename=outPath)
    rr <- resample(mos, templateR, filename= outPath , method ='ngb')
  }
}