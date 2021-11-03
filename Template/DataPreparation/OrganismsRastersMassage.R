
#################################
###  Author : Ross Searle         
###  Date : Tue Jan 28 11:59:04 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : massage the Organisms covariate rasters
###  Notes : 
#################################

library(raster)
library(stringr)
library(rgdal)

rootDir <- '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/Organisms'
outRoot <-  'E:/Roper/DataMassage/Organisms'
if(!dir.exists(outRoot)){dir.create(outRoot)}
fls <- list.files(rootDir, '*.tif$', full.names = T)

#these Supplied by Seonaid on the 9/1/20
Top <- -12.894
Bottom <- -16.778
Left <- 131.995
Right <- 135.7075
rext <- extent(Left, Right, Bottom, Top ) 
rext

if(!dir.exists( paste0( outRoot, '/crp/'))){dir.create(paste0( outRoot, '/crp/'))}
for(a in 1:length(fls)){
  print(fls[a])
  outPath <- paste0( outRoot, '/crp/', basename(fls[a]))
  if(!file.exists(outPath)){
    mos <- raster(fls[a])
    c1  <- crop(mos, rext, snap='out', filename=outPath)
    # rr <- resample(c1, templateR, filename=  , methods[a])
  }
}



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