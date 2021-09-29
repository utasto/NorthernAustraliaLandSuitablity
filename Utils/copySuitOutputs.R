#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 13:11:31 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Copy the suitability rasters to their final project location
#################################


library(readxl)


rootDir <- 'Y:/Ross/Roper'
source(paste0(rootDir, '/Scripts/RoperLandSuit/Suitability/SuitUtils_Roper.R'))

suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
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

for (i in 1:length(useList)) {
  
  use <- useList[i]
  sFile <- paste0(inDir, '/', use, '.tif')
  sFileU <- paste0(inDir, '/', use, '_Uncert.tif')
    if(file.exists(sFile)){
      dFile <- paste0(outDir, '/', use, '.tif')
      dFileU <- paste0(outDir, '/', use, '_Uncert.tif')
      if(!file.exists(dFile)){
        print(paste0('Copying ', basename(sFile), ' to ', dFile))
        file.copy(sFile,dFile, overwrite = T)
        file.copy(sFileU,dFileU, overwrite = T)
      }
    }
  }
  
