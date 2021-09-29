
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 12:14:38 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Prepare the ANU Climate rasters for use  in DSM
#################################


library(akima)
library(raster)
library(rgdal)
library(stringr)

templateR <- raster('E:/Roper/DataMassage/DEM/demcrop.tif')
outDir <- 'C:/Temp/ANUClim'

Top <- -12.894
Bottom <- -16.778
Left <- 131.995
Right <- 135.7075
rext <- extent(Left, Right, Bottom, Top ) 
rext


# Crop the ANU Climate rasters

rootDir <- 'P:/CoVariates/Climate/newClimatics'
fls <- list.files(rootDir, full.names = T,  '*.tif')

for (i in 1:length(fls)){
  outfile <- paste0('c:/temp/anucrop', '/', basename(fls[i]))
    if(!file.exists(outfile)){
        print(fls[i])
        inR <- raster(fls[i])
        moscrop  <- crop(inR, templateR, snap='out', filename = outfile)
       # rr <- resample(inR, templateR, method='ngb', filename = paste0(outDir, '/', str_replace(basename(fls[i]), '.txt', '.tif' )), overwrite = T)
        plot(moscrop)
    }
}

# Resample the ANU Climate rasters

rootDir <- 'c:/temp/anucrop'
fls <- list.files(rootDir, full.names = T,  '*.tif')

for (i in 1:length(fls)){
  outfile <- paste0('c:/temp/anuResample', '/', basename(fls[i]))
  if(!file.exists(outfile)){
    print(fls[i])
    inR <- raster(fls[i])
    #moscrop  <- crop(inR, templateR, snap='out', filename = outfile)
     rr <- resample(inR, templateR, method='ngb', filename = outfile, overwrite = T)
    plot(moscrop)
  }
}

