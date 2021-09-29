library(raster)
library(doParallel)

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')
rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'


catchmentName <- 'Darwin'

# limList <- c('Climate-annual_rainfall'
#              , 'Climate-heat_dry_season_and_perennial'
#              , 'Climate-heat_wet_season_and_perennial'
#              , 'Climate-frost'
#              , 'Climate-temp_variation'
#              , 'Wetness'
#              #, 'Moisture_availability_1.5'
#              , 'Moisture_availability_1.0'
#              #, 'Moisture_availability_0.6'
#              , 'Nutrient_balance'
#              , 'Soil_depth'
#              , 'Rockiness'
#              , 'Microrelief'
#              , 'Irrigation_efficiency'
#              #, 'Irrigation_efficiency-high_rate_methods'
#              #, 'Moisture_availability_RAINFED_CROPPING'
#              , 'Physical_restrictions'
#              , 'Erosion'
#              , 'Salinity_surface'
#              , 'Acid_Sulfate_Soil_Potential'
#              )


landuse <- 'Cane!wet-long!spr'

suitFile <- paste0(catchmentName, '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

l <- getAvailableLimitations(catchmentName, landuse, suitFramework)
currentLims <- l[l$avail & l$correctCnt,][,1]


for(j in 1:length(currentLims)){
  
  lim <- currentLims[j]
  print(paste0('Mapping ', lim))
  limDir <- paste0(rootDir, '/Limitations/',catchmentName, '/',  lim)
  usePathList <- list.dirs(path = limDir, full.names = TRUE, recursive = F)
  
  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  templateR <- raster(paste0(rootDir,'/Attributes/AWC150/',catchmentName,'/AWC150_',catchmentName,'.tif'))
  doMapSubClasses(rootDir, catchment=catchmentName, limitation=lim, landuse, templateR)
}


#   
#     cl<-makeCluster(numcpus,outfile="")
#     registerDoParallel(cl)
#     foreach(i=1:length(usePathList), .packages=c('raster')) %dopar%  doMapSubClasses(rootDir = rootDir, limitation = lim, usePath = usePathList[i], templateR = templateR, bs=bs)
#     stopCluster(cl)
# }
