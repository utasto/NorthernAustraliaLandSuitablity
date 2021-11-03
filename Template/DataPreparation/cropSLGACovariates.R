
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 13:28:08 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Crop, resample and mask all the SLGA 90m data stack to the project area
#################################

library(raster)
library(stringr)
library(rgdal)

library(raster)

m <- readRDS('C:/Projects/Roper/Models/asc.mod.2.rds')
m

rootDir = 'C:/Projects/Roper'
covariates.rootDirPath  = 'D:/TERNSoils/CoVariates'
covariates.outrootDirPath = paste0(rootDir, '/Covariates')


clipArea <- raster('C:/Projects/Roper/Boundaries/roper_srtm_derived_catchment_wgs84.tif')
tr <- raster('D:/TERNSoils/CoVariates/AusMask.tif')
b1 <- crop(tr, clipArea)
b2 <- resample(clipArea, b1, method = 'ngb')
cr <- crop(tr, b2)
mr <- mask(cr, b2, filename=outfile, overwrite=T)
plot(mr)
writeRaster(mr, 'C:/Projects/Roper/Boundaries/roper_srtm_derived_catchment_wgs84.tif', overwrite=T)


clipArea <- raster('C:/Projects/Roper/Boundaries/roper_srtm_derived_catchment_wgs84.tif')
covsToUse = list.files( paste0(covariates.rootDirPath), pattern = '.tif$', full.names = T, recursive = T)

theStack = stack()
for (i in 118:length(covsToUse)) 
{
  rname <-covsToUse[i]
  if(file.exists(rname)){
   
    r = raster(rname)
    if(basename(dirname(rname)) != "Associated" & basename(dirname(rname)) != "Hillshades"){
      
      print(paste0(i, ' ', rname))
       outDir <- paste0(covariates.outrootDirPath)
       
       
       outfile <- paste0(outDir, '/', basename(rname) )
       cr <- crop(r, clipArea)
       mr <- mask(cr, clipArea, filename=outfile, overwrite=T)
      theStack<- addLayer(theStack,r)
    }
  }
}
nlayers(theStack)
