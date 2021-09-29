
catchment <- 'Fitzroy'
catchment <- 'Darwin'
catchment <- 'Mitchell'

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
rootDirSweeps = paste0(rootDir, '/suitSweeps')

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')


#landuses <- "PriorityUses"

landuses <- "ReRuns"


suitFile <- paste0(catchment, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
#usesDF <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/PriorityLanduses.xlsx'), sheet='PriorityUses', col_names = T))
#uses <- usesDF$Darwin

outDir <- paste0(rootDirSweeps, '/', catchment, '/', landuses)
dir.create(outDir, recursive = T)


#uses <- c('Macadamia!per!mini-spr', 'Almond!per!mini-spr', 'Avocado!per!mini-spr', 'Coffee!per!mini-spr', 'Indian S!per!mini-spr', 'Citrus!per!mini-spr', 'Lychee!per!mini-spr')
#uses <- c('Cane!wet-long!fur', 'Cane!wet-long!rainfed', 'Cane!wet-long!spr','Rhodes!wet-long!fur', 'Rhodes!wet-long!spr')

###  ps and pa reruns
uses <- c('Cotton!dry!fur','Cotton!dry!spr','Cotton!wet!fur','Cotton!wet!spr','Fr-sorgh!wet!fur','Fr-sorgh!wet!spr','Fr-sorgh!wet!rainfed','Fr-sorgh!dry!fur'
          ,'Fr-sorgh!dry!spr','Rhodes!wet-long!fur','Rhodes!wet-long!spr','Lablab!wet!fur','Lablab!wet!spr','lablab!wet!rainfed','Lablab!dry!fur'
          ,'Lablab!dry!spr','Maize-sil!wet!fur','Maize-sil!wet!spr','Maize-sil!wet!rainfed','Maize-sil!dry!fur','Maize-sil!dry!spr','Indian-Sand!per!fur'
          ,'Indian-S!per!mini-spr','African-M!per!fur','African-M!per!tri','Teak!per!fur','Teak!per!tri','Cashew!per!spr','Cashew!per!tri','Mango!per!spr'
          ,'Mango!per!tri','Banana!per!spr','Banana!per!tri','Citrus!per!mini-spr','Coffee!per!mini-spr','Avocado!per!mini-spr','Lychee!per!mini-spr'
          ,'Papaya!per!spr','Papaya!per!tri','Macadamia!per!mini-spr','Almond!per!mini-spr')




ID <- numeric(length((uses)))
Luse <- character(length((uses)))
Catchment <- character(length((uses)))

for (i in 1:length(uses)){

  ID[i] <- i
  Luse[i] <- uses[i]
  Catchment <- catchment
  }
  

df <- data.frame(ID, Luse, Catchment)
#write.csv(df, paste0(rootDirSweeps, '/sweeps_', catchment, '_',landuses, '.csv') ,row.names=FALSE)
write.csv(df, paste0(rootDirSweeps, '/sweeps_', catchment, '_', 'Reruns.csv') ,row.names=FALSE)



######  Write out the Pearcey shell sbatch script ######

fname <- paste0(rootDirSweeps, '/', catchment, '-', landuses, '.sh')

       unlink(fname)
       fileConn<-file(fname, open = 'wb')
       writeLines('#!/bin/bash', fileConn)
       writeLines('#SBATCH --job-name="F_Suit"', fileConn)
       writeLines('#SBATCH --nodes=1', fileConn)
       writeLines('#SBATCH --ntasks=20', fileConn)
       writeLines('#SBATCH --time=5:40:00', fileConn)
       writeLines('#SBATCH --mem=36GB', fileConn)
       writeLines('#SBATCH --exclusive', fileConn)
       writeLines(paste0('#SBATCH -a 1-',length(uses)), fileConn)
       writeLines('#SBATCH -o /OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/suitSweeps/Fitzroy/err/out_%j_%a.txt', fileConn)
       writeLines('#SBATCH -e /OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/suitSweeps/Fitzroy/err/error_%j_%a.txt', fileConn)
       writeLines('module load R/3.4.0', fileConn)
       writeLines(paste0('Rscript p_generateSuitsHPC.R ', catchment, ' ', landuses, ' $SLURM_ARRAY_TASK_ID'), fileConn)

       close(fileConn)
       print(paste0('Sweep shell script file written to ', fname))

       


