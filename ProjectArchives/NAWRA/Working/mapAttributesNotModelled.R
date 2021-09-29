
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')

# args = commandArgs(trailingOnly=TRUE)
# catchmentName <- args[1]
# attribute <- args[2]

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

catchmentName <- 'Darwin'
attribute <- 'Rain'
attribute <- 'Slope'

templateR <- raster(paste0(rootDir,'/Attributes/AWC150/', catchmentName, '/AWC150_', catchmentName, '.tif'))
bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))


fname <- doMapAttributes(rootDir, attribute, catchmentName, templateR, bs)
rs <- raster(fname)
plot(rs)
