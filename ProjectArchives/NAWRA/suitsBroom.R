

TESTING <- F

catchment <- 'Darwin'
catchment <- 'Fitzroy'
catchment <- 'Mitchell'

if(TESTING == T){
  rootDirSweeps = 'D:/temp/sweeptest'
  rootDir = 'D:/Projects/NAWRA/Production'
}else{
 
  rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
  rootDirSweeps = paste0(rootDir, '/suitSweeps')
}

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')


#landuses <- "PriorityUses"
landuses <- "Specific"
landuses <- "Alluses"
landuses <- 'ReRuns'

suitFile <- paste0(catchment, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

#uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
#usesDF <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/PriorityLanduses.xlsx'), sheet='PriorityUses', col_names = T))
#uses <- usesDF$Darwin

uses <- c('Cotton!dry!fur','Cotton!dry!spr','Cotton!wet!fur','Cotton!wet!spr','Cotton!wet!rainfed','Fr-sorgh!wet!fur','Fr-sorgh!wet!spr','Fr-sorgh!wet!rainfed','Fr-sorgh!dry!fur'
          ,'Fr-sorgh!dry!spr','Rhodes!wet-long!fur','Rhodes!wet-long!spr','Lablab!wet!fur','Lablab!wet!spr','lablab!wet!rainfed','Lablab!dry!fur'
          ,'Lablab!dry!spr','Maize-sil!wet!fur','Maize-sil!wet!spr','Maize-sil!wet!rainfed','Maize-sil!dry!fur','Maize-sil!dry!spr','Indian-Sand!per!fur'
          ,'Indian-S!per!mini-spr','African-M!per!fur','African-M!per!tri','Teak!per!fur','Teak!per!tri','Cashew!per!spr','Cashew!per!tri','Mango!per!spr'
          ,'Mango!per!tri','Banana!per!spr','Banana!per!tri','Citrus!per!mini-spr','Coffee!per!mini-spr','Avocado!per!mini-spr','Lychee!per!mini-spr'
          ,'Papaya!per!spr','Papaya!per!tri','Macadamia!per!mini-spr','Almond!per!mini-spr')



outDir <- paste0(rootDirSweeps, '/', catchment, '/', landuses)
dir.create(outDir, recursive = T)


#uses <- c('Macadamia!per!mini-spr', 'Almond!per!mini-spr', 'Avocado!per!mini-spr', 'Coffee!per!mini-spr', 'Indian S!per!mini-spr', 'Citrus!per!mini-spr', 'Lychee!per!mini-spr')
#uses <- c('Cane!wet-long!fur', 'Cane!wet-long!rainfed', 'Cane!wet-long!spr','Rhodes!wet-long!fur', 'Rhodes!wet-long!spr')

for (i in 1:length(uses)){

    
   
   sname <- paste0('Suits_', i)
    fname <- paste0(outDir, '/',sname, '.bat')
    
          unlink(fname)
          fileConn<-file(fname, open = 'a')
          writeLines('@echo off', fileConn)
          writeLines('echo %CCP_NUMCPUS%', fileConn)
          writeLines('set OMP_NUM_THREADS=%CCP_NUMCPUS%', fileConn)
          writeLines('set PATH=%PATH%;C:\\winapps\\R\\3.1.2\\bin\\x64', fileConn)
          writeLines(paste0('rscript \\\\OSM-09-CDC.it.csiro.au\\OSM_CBR_LW_SLGA_work\\projects\\NAWRA\\production\\scripts\\suitability\\generateSuitsHPC.R ', catchment, ' ',uses[i]), fileConn)
          close(fileConn)
          print(paste0(i, ' : Sweep batch file written to ', fname))
      
  }
  

 




