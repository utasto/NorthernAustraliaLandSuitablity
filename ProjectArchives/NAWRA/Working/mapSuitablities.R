
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')

catchmentName <- 'Mitchell'  

  rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
  AllUses <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/PriorityLanduses.xlsx'), sheet='PriorityUses', col_names = T))
  
  templateR <- raster(paste0(rootDir,'/Attributes/AWC150/', catchmentName, '/AWC150_', catchmentName, '.tif'))
  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  pUses <- AllUses[,catchmentName]
  
  uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  pUses <- uses
  
  for(i in 1:length(pUses)){
    landuse <- pUses[i]
    print(landuse)
    fnames <- doMapSuitabilities(rootDir, landuse, catchment=catchmentName, templateR, bs)
  }
  