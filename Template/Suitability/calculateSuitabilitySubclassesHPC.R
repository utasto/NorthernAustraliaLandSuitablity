#################################
###  Author : Ross Searle         
###  Date : Thu Sep 30 09:05:43 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Calculates the suitability subclasses for each of the limitations in the framework - function is in suitUtils.R
#################################


library(raster)
library(plyr)
library(fst)
library(tictoc)
library(stringr)


args = commandArgs(trailingOnly=TRUE)

k = args[1]
lim <- args[2]
useToDo <- args[3]

#lim <- 'Wetness'
#useToDo <- "Mango!per!tri"
uses <- str_split(useToDo, '=')[[1]]
#uses <- uses[1]
print(paste0('Doing subclasses for ',uses))
tic()

# dataRootDir = 'Y:/Ross/Roper'
# scriptRootDir = 'Y:/Ross/Roper/Scripts/RoperLandSuit'
# limRoot <- 'N:/3_Land_suitability/0_Working/Uta/Roper'
dataRootDir = '/datasets/work/af-digiscapesm/work/Ross/Roper'
scriptRootDir = '/home/sea084/roper/Scripts/RoperLandSuit'
limRoot <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
#limRoot <- '/Scratch1/sea084/Roper'
#outRoot <- '/scratch1/projects/slga/Roper/Outputs'


source(paste0(scriptRootDir, '/Suitability/SuitUtils_Roper.R'))
source(paste0(scriptRootDir, '/Mapping/RandomForestUtils_Roper.R'))

suitFile = paste0(limRoot, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(limRoot, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(limRoot, '/Suitability/LimitationsFileMappingsRoper2.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

limInfo <- suitFramework[suitFramework$LIM_Name==lim,]
#write.csv(limInfo, paste0(limRoot, '/Suitability/LimInfo.csv'), row.names = F)



tic()
print(paste0('Started at - ', Sys.time()))
print(lim)
calcSubClasses(k, lim, uses, suitFramework, limRastersList, limInfo, limRoot)
print(toc())


print('Job successfully completeed')