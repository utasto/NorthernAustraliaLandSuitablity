library(raster)
library(doParallel)
library(plyr)
library(stringr)

setwd('/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/Scripts/Pearcy')

source('RandomForestUtils_V3.R')
source('SuitabilityUtils_V2.R')
source('mappingUtils.R')


args = commandArgs(trailingOnly=TRUE)

catchmentName <- args[1]
useToDo <- args[2]
batchID <- args[3]


rootDir <- '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production'

#     catchmentName <- 'Darwin'
#     useToDo <- 'PearcyTest'
#     batchID <- 3



sname <- paste0('sweeps_', catchmentName, '_', useToDo, '.csv')

inDF <- read.csv(paste0(rootDir, '/suitSweeps/', sname ))

srow <- inDF[inDF$ID==batchID, ]
landuse <- srow$Luse


suitfile <- paste0(catchmentName, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')


# catchments <-  c('Mitchell', 'Darwin', 'Fitzroy')
# uses <- readSuitLanduses(rootDir = rootDir, suitFile = 'NAWRA-Suitability_Subclasses_single_sheet_29-03-17.xlsx', classFile = 'LimitationRanges.xlsx')

l <- getAvailableLimitations(catchmentName, landuse, suitFramework)
#currentLims <- l[l$avail & l$correctCnt,]
currentLims <- l[l$avail,]

outDir <- paste0(rootDir, '/Suitability/', catchmentName, '/', landuse, '/Chunks')
dir.create(outDir, recursive = T)

bs  <- readRDS(paste0(rootDir, '/Masks/chunks_' , catchmentName,'.rds'))

calculateSuitabilities(rootDir, landuse, catchmentName, currentLims, outDir)




