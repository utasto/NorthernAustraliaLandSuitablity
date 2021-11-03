
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 16:51:32 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Calculates the overall suitability by reading in all the subclass chunks - 
###  across all limitations and working out the max limitation value  - function is in suitUtils.R
#################################

library(raster)
library(stringr)
library(rgdal)

library(plyr)
library(raster)
library(doParallel)
library(stringr)
library(fst)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)

k= as.numeric(args[1])
useToDo <- args[2]
#uses <- str_split(useToDo, '_')[[1]]
uses <- useToDo 
print(uses)


source(paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Scripts/RoperLandSuit/Suitability/SuitUtils_Roper.R'))

rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
limRoot <- '/datasets/work/af-digiscapesm/work/Ross/Roper'


suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')


suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
useList <- colnames(suitFramework)[15:ncol(suitFramework)]

#limsToDo <- unique(suitFramework$LIM_Name)#[-c(6, 19)]
limsToDo <- unique(suitFramework$LIM_Name)[-c(9,14)]


for(i in length(uses)){
  use<-uses[i]

print('######################')
print(use)
print('######################')

  tic()

  print(paste0('Calculating overall Suits for ', use))
  outDir <- paste0(limRoot, '/Suitability_V2/Chunks/', use)
  if(!dir.exists(outDir)) dir.create(outDir, recursive = T)

  calculateOverallSuitabilitesModal(limRoot=limRoot, landuse=use, currentLims=limsToDo, outDir, k)
  print(toc())

print(paste0('Finished suit calcs at ', as.character(Sys.time())))


}




