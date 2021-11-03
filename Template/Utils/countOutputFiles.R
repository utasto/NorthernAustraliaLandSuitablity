
#################################
###  Author : Ross Searle         
###  Date : Thu Sep 30 12:05:14 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Another file counter
#################################


rootDir <- '/datasets/work/af-digiscapesm/work/Ross/Roper'
scriptsPath <- '/home/sea084/roper/Scripts/RoperLandSuit/HPC'
outPath <- '/home/sea084/roper/Outputs'

source(paste0(scriptsPath, '/HPCUtils.R'))


suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(rootDir, '/Suitability/LimitationsFileMappingsRoper.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

useList <- colnames(suitFramework)[15:ncol(suitFramework)]
allLims <- unique(suitFramework$LIM_Name)

dirs <- list.dirs(paste0(outPath, '/Subclasses'), recursive = F, full.names = F)
setdiff(allLims,dirs)

merge(data.frame(dirs, c('dirs')), data.frame(allLims, c('LimList')), by.x='dirs', by.y='allLims', all=T)

type <- 'Subclasses'
att <- "Climate-frost"

numUses <- 58
numFiles <- 700
dfl <- vector(mode='list', length(allLims))

for (i in 1:length(allLims)) {
  att <- allLims[i]
  df <- countOutputFiles(outPath, type, att, numUses, numFiles)
  
  totFiles = numFiles*numUses
  if(sum(df$fCount) == totFiles){
    print(paste0(att, ' - OK'))
  }else{
    n <-nrow(df[df$fCount < numFiles, ])
    print(paste0(att, ' - PROBLEM   ', n, ' uses are missing files'))
  }
  dfl[[i]]<- df
}

