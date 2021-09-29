library(raster)
library(doParallel)
library(plyr)
library(stringr)

TESTING <- F

machineName <- Sys.info()['nodename'] 
if(machineName == 'MARANA-DP'){
  args <- c('Darwin', 'Irrigation_efficiency', 'Cane_wet_rainfed;Poppies_dry_fur;Rice_dry_flood' )
  rootDir = 'D:/Projects/NAWRA/Production'
  source('C:/Users/sea084/Dropbox/RossRCode/NAWRA/Production/SuitabilityUtils_V2.R')
  numcpus = 3
  
}
if(TESTING == T){
  args <- c('Mitchell', 'Surface_Texture', 'Cane!wet-long!rainfed;Poppies!dry!fur;Rice!dry!flood' )
  #args = commandArgs(trailingOnly=TRUE)
  rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
  source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
  source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/RandomForestUtils_V3.R')
  numcpus = 1
} else {
  
  args = commandArgs(trailingOnly=TRUE)
 # rootDir = '//osm-09-cdc.ib.hpc/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
 # source('//osm-09-cdc.ib.hpc/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
 # source('//osm-09-cdc.ib.hpc/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/RandomForestUtils_V3.R')
 
 rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
  source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
  source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/RandomForestUtils_V3.R')
 
  numcpus = detectCores()
}



catchmentName <- args[1]
lim <- args[2]
landuses <- args[3]
useList <- unlist(c(str_split(landuses, ';')))



suitfile <- paste0(catchmentName, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
limRastersList <- getLimitationsPaths(rootDir = rootDir, mapFile = 'LimitationsFileMappings.xlsx')


for(i in 1: length(useList)){
  outDir <- paste0(rootDir, '/Limitations/', catchmentName, '/', lim, '/', useList[i],  '/Chunks')
  #if(!dir.exists(outDir)){
    dir.create(outDir, recursive = T)
  #}
}

bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]


cl<-makeCluster(numcpus,outfile="")
registerDoParallel(cl)
foreach(k=1:length(bs$row)) %dopar% processSubClassesParallel(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)
#foreach(k=1:2) %dopar% processSubClassesParallel(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)

stopCluster(cl)

