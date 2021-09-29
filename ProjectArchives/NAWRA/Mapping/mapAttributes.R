
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')

# args = commandArgs(trailingOnly=TRUE)
# catchmentName <- args[1]
# attribute <- args[2]

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

catchmentName <- 'Mitchell'
attribute <- 'DEM'
attribute <- 'HotDry'

templateR <- raster(paste0(rootDir,'/Attributes/AWC150/', catchmentName, '/AWC150_', catchmentName, '.tif'))
bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))


attributeList <- c('HotDry', 'HotWet', 'Frost', 'Rain', 'Slope')

for(i in 1:length(attributeList)){

  attribute <- attributeList[i]

  doMapAttributes(rootDir, attribute, catchmentName, templateR, bs)

}