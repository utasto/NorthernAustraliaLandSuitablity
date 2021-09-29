library(raster)
library(doParallel)
library(plyr)
library(stringr)

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/RandomForestUtils_V3.R')
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')

args = commandArgs(trailingOnly=TRUE)

rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
#rootDir <- '//osm-09-cdc.ib.hpc/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

    # catchmentName <- 'Fitzroy'
    # landuse <- 'Papaya!per!spr'
landuse <- 'Rice-upland!wet!spr'
landuse <- 'Rice-upland!wet!rainfed'
landuse <- 'Fr-sorgh!wet!fur'

 catchmentName <- args[1]
 landuse <- args[2]

suitfile <- paste0(catchmentName, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')


# catchments <-  c('Mitchell', 'Darwin', 'Fitzroy')
# uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

l <- getAvailableLimitations(catchmentName, landuse, suitFramework)
#currentLims <- l[l$avail & l$correctCnt,]
currentLims <- l[l$avail,]

outDir <- paste0(rootDir, '/Suitability/', catchmentName, '/', landuse, '/Chunks')
dir.create(outDir, recursive = T)

bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))

rasterFile <- paste0(rootDir, '/Suitability/', catchmentName, '/', landuse, '$',catchmentName, '$suitability_Cat.tif' )

#if(!file.exists(rasterFile)){
  
  calculateSuitabilities(rootDir, landuse, catchmentName, currentLims, outDir)
  
#}






