#################################
###  Author : Ross Searle         
###  Date : Thu Sep 30 09:09:24 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Checks the file counts of the suitability subclass chunks
#################################


library(readxl)

scriptsPath <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git'
source(paste0(scriptsPath, '/RoperLandSuit/Suitability/SuitUtils_Roper.R'))

rootDir <- 'X:/3_Land_suitability/0_Working/Uta/Roper'
subDir <- 'Z:/Ross/Roper/Subclasses'

suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V4.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
currentLims <- unique(suitFramework$LIM_Name)
useList <- getSuitList(suitFramework)


fdf <- checkSubclassFiles(subClassDir=subDir, limitations=currentLims, uses=useList, reqCnt=700)
write.csv(fdf, 'c:/temp/files.csv')

fdf[fdf$Count!=700, ]

checkSubclassFiles <- function(subClassDir, limitations, uses, reqCnt){

 outdf <- data.frame(Lim=character(), use=character(), Count=numeric(), OK=logical())
  for (i in 1:length(limitations)) {
    lim <- limitations[i]
    print(lim)
    for (j in 1:length(uses)) {
      use <- uses[j]
      dir <- paste0(subClassDir, '/',lim, '/', use, '/Chunks')
      fls <- list.files(dir, pattern = '.fst')
      df <- data.frame(Lim=lim, use=use, Count=length(fls), OK=length(fls)==reqCnt)
      outdf <- rbind(outdf, df)
    }
  }
 return(outdf)
}
