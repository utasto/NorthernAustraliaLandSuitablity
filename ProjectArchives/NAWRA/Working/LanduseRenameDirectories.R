rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Limitations'

CatchmentName <- 'Fitzroy'
dirs <- list.dirs(paste0(rootDir, '/', CatchmentName), full.names = T, recursive = F)

origs <- c('Macadamia!per!tri',        'Almond!per!tri',      'Avocado!per!tri',      'Coffee!per!tri',      'Indian-S!per!tri',      'Citrus!per!tri',      'Lychee!per!tri')
renames <- c('Macadamia!per!mini-spr', 'Almond!per!mini-spr', 'Avocado!per!mini-spr', 'Coffee!per!mini-spr', 'Indian-S!per!mini-spr', 'Citrus!per!mini-spr', 'Lychee!per!mini-spr')

 origs <- c('Indian S!per!mini-spr')
 renames <- c('Indian-S!per!mini-spr')

 origs <- c('Cane!wet!fur', 'Cane!wet!rainfed', 'Cane!wet!spr')
 renames <- c('Cane!wet-long!fur', 'Cane!wet-long!rainfed', 'Cane!wet-long!spr')

 
 origs <- c("Rhodes!wet!fur", "Rhodes!wet!spr")
 renames <- c("Rhodes!wet-long!fur", "Rhodes!wet-long!spr")

for (i in 1: length(dirs)){
  
  ldirs <- list.dirs(paste0(dirs[i]), full.names = T, recursive = F)

  for(j in 1:length(ldirs)){
      for(k in 1:length(origs)){
        if(basename(ldirs[j]) == origs[k]){
          newname <- paste0(dirname(ldirs[j]), '/', renames[k] )
          print('Renaming')
          print(ldirs[j])
          print( newname)
          file.rename(ldirs[j],newname)
        }
      }
  }
}
 
 
 
 
 
 rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
 suitFile <- paste0(CatchmentName, '_Suitability_Framework.xlsx')
 uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
 
 
 #tDir <- "//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Limitations/Fitzroy/Climate-frost" 
 tDir <- "//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Suitability/Fitzroy" 
 ldirs <- list.dirs(tDir, full.names = F, recursive = F)
 
 setdiff(ldirs, uses )
 


