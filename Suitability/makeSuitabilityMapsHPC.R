
#################################
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:23:55 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Generates suitability maps from the suitability chunk files
#################################


library(raster)
library(doParallel)
library(plyr)
library(stringr)
library(fst)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)


k <- as.numeric(args[1])
chunkLines=20

source(paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Scripts/RoperLandSuit/Suitability/SuitUtils_Roper.R'))


dataDir <- '/datasets/work/af-digiscapesm/work/Ross/Roper'
rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
templateR<- raster(paste0(dataDir, '/Boundaries/RoperMask.tif'))

 suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
 classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
 suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
 useList <- colnames(suitFramework)[15:ncol(suitFramework)]
 use <- useList[k]
 
#bs  <- readRDS('/datasets/work/af-digiscapesm/work/Ross/Roper/Outputs/blocks.dat')

chk <-getChunkInfo(chunkLines, nrow(templateR))

tic()

  print(paste0('Making suitability map for ', use))

  generateSuitRaster(dataDir, templateR = templateR, use=use)

toc()
print(paste0('Finished making suitability map at ', as.character(Sys.time())))




