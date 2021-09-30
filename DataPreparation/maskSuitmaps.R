#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:31:26 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Mask and copy the output suitablity maps to a location
#################################


library(raster)


rasterOptions(progress = 'text')

rootDir <- 'Y:/Ross/Roper'
outDir <- paste0('N:/3_Land_suitability/0_Working/Ross/SuitsDraft1')

source(paste0(rootDir, '/Scripts/RoperLandSuit/Suitability/SuitUtils_Roper.R'))

suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V4.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(rootDir, '/Suitability/LimitationsFileMappingsRoper.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

useList <- colnames(suitFramework)[15:ncol(suitFramework)]
limsToDo <- unique(suitFramework$LIM_Name[suitFramework$NumQuals==1])


inDir <- paste0(rootDir, '/Outputs/Suitability')
fls <- list.files(inDir, '*.tif')
length(fls)
outDir <- paste0('N:/3_Land_suitability/0_Working/Ross/SuitsDraft1')

mr <- raster(paste0(rootDir, '/Boundaries/bdyBoxWithNaRiver.tif'))

for (i in 1:length(useList)) {
  
  use <- useList[i]
  inRasterPath <- paste0(rootDir, '/Outputs/Suitability/',  use, '.tif')
  maskRasterPath <- paste0(outDir, '/',  use, '_m.tif')
  if(!file.exists(maskRasterPath)){
    
    print(paste0('Masking Suitability raster : ', use))
    inr1 <- raster(inRasterPath) 
    r1 <- mask(inr1, mr, filename=maskRasterPath)
  }
  
}





