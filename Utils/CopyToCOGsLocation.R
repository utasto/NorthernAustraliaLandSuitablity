#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 13:13:59 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Copies suitability rasters to the location required for the Shiny Map Viewer. It then generates COGs for use in the Mapviewer
### Notes : run on Pearcey interactive node
#################################

library(stringr)

####  Copies suitability rasters to the location required for the Shiny Map Viewer #########

rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V4.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(rootDir, '/Suitability/LimitationsFileMappingsRoper2.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

useList <- getSuitList(suitFramework)
limsToDo <- unique(suitFramework$LIM_Name[suitFramework$NumQuals==1])

lims <- unique(limRastersList$ChunkName)
lims <- lims[-c(1:5)]


limpaths <- paste0(rootDir, '/Maps/', lims, '/', lims, '.tif')
limpaths 


for (i in 1:length(limpaths)) {
  
  f <- limpaths[i]
  toFile <- paste0('/datasets/work/lw-soildatarepo/work/Ross/Roper/Maps/Attributes/', basename(f))
  file.copy(f, toFile)
  
}



  ############  generates COGs for use in the Mapviewer  ##################

rootDir <- '/datasets/work/lw-soildatarepo/work/Ross/Roper/Maps'


fls <- list.files(paste0(rootDir, '/Attributes'), pattern = '.tif$', full.names = T, recursive = F)

for (i in 1:length(fls)) {
  cat(paste0(i, ' '))
  oname <- paste0(rootDir, '/Attributes_COGs/', str_remove(basename(fls[i]), '.tif'), '_COG.tif')
  cmd <- paste0('gdal_translate ', fls[i], ' ', oname,' -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW' )
  system(cmd)
}