
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:34:40 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Preprocess the climate rasters
#################################

library(raster)
rasterOptions(progress = 'text')

rootDir = 'E:/temp/RoperClimateCovs/90'
covariates.outrootDirPath <- 'E:/temp/RoperClimateCovs/Cropped'


clipArea <- raster('N:/3_Land_suitability/4_Data/covariates/all/Relief_demcrop.tif')


templateR <- raster('E:/Roper/Relief_demcrop.tif')

covsToUse = list.files( rootDir, pattern = '.tif$', full.names = T, recursive = T)

theStack = stack()
for (i in 1:length(covsToUse)) 
{
  rname <-covsToUse[i]
  if(file.exists(rname)){
    
    r = raster(rname)
      print(paste0(i, ' ', rname))
      outDir <- paste0(covariates.outrootDirPath)
      outfile <- paste0(outDir, '/', basename(rname) )
      cr <- crop(r, clipArea)
      rr <- resample(cr, templateR, method='ngb')
      mr <- mask(rr, templateR, filename=outfile, overwrite=T)
      theStack<- addLayer(theStack,r)
    
  }
}
nlayers(theStack)




covsToUse = list.files( 'M:/work/datasets/national/covariates/mosaics/30m', pattern = '.tif$', full.names = T, recursive = T)
covariates.outrootDirPath <- 'E:/temp/RoperClimateCovs/CroppedAll'


for (i in 1:length(covsToUse)) 
{
  rname <-covsToUse[i]
  if(file.exists(rname)){
    
    r = raster(rname)
    print(paste0(i, ' ', rname))
    outDir <- paste0(covariates.outrootDirPath)
    outfile <- paste0(outDir, '/', basename(rname) )
    cr <- crop(r, clipArea, filename=outfile, overwrite=T)
    cr <- NULL
    r <- NULL
    gc()
    
  }
}


