
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
lim <- 'Irrigation_efficiency'
catchmentName <- 'Darwin'
use <- 'Cane!wet-long!fur'
  
  
fname <- doMapSubClasses(rootDir = rootDir, catchment = catchmentName, limitation = lim, landuse = use, templateR = templateR)
r <- raster(fname)
plot(r)






d <- readRDS('Q:/Projects/NAWRA/Production/Limitations/Darwin/Irrigation_efficiency/Cane!wet-long!fur/Chunks/subclass_Suit_10.rds')
