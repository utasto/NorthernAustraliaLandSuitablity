library(raster)
library(doParallel)
library(plyr)
library(stringr)


##### debug on TernSoils VM   ####
     # rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
     # rootDirSweepsUnix = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Pearcy/LimitationSweeps'
     # source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
     # source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/RandomForestUtils_V3.R')
     # 
     # catchmentName <- 'Mitchell'
     # lim <- 'Surface_Condition'
     # landuseNo <- as.numeric('1')
#######################################

  args = commandArgs(trailingOnly=TRUE)
  rootDir = '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production'
  rootDirSweepsUnix = '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/Pearcy/LimitationSweeps'
  
  source('/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
  source('/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/Scripts/Utils/RandomForestUtils_V3.R')
  
  catchmentName <- args[1]
  lim <- args[2]
  landuseNo <- as.numeric(args[3])
  
  
  

outDirUnix <- paste0(rootDirSweepsUnix, '/', catchmentName, '/', lim)
#fnameUses <- paste0(outDirUnix, '/Uses4Lim_',lim, '.csv')
fnameUses <- paste0(outDirUnix, '/Uses4Lim_',lim, '_reruns.csv')

uses4Lim <- read.csv(fnameUses, header=TRUE)

theuse <- as.character(uses4Lim[landuseNo, 2])
useList <- c(theuse)
#useList <- unlist(c(str_split(landuses, ';')))



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

bs  <- readRDS(paste0(rootDir, '/Masks/chunks_' , catchmentName,'.rds'))
limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]

# for(k in 1:length(bs$row)){
# processSubClassesParallel(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)
# }

#numcpus=1
numcpus <- detectCores()
cl<-makeCluster(numcpus,outfile="")
registerDoParallel(cl)
foreach(k=1:length(bs$row)) %dopar% processSubClassesParallel(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)
#foreach(k=1:2) %dopar% processSubClassesParallel(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)

stopCluster(cl)

print(paste0('Finished ', lim, ' number ', landuseNo))

