#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 13:26:01 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Counts the number of files in the various subclass directories to check that they are all there 
#################################



###### Count per limitation  ########

lim <- 'Climate-heat_dry_season_and_perennial'
lim <- 'Climate-heat_wet_season_and_perennial'

dataRootDir = paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Subclasses/', lim)

dirs <- list.dirs(dataRootDir, full.names = T, recursive = F)


for (i in 1:length(dirs)) {
  
  fls <- list.files(paste0(dirs[i], '/Chunks'))
  l <- length(fls)
  print(paste0(lim, ' : ', basename(dirs[i]), ' = ', l ))
  
}



###### Per landuse  ########

dataRootDir = paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Subclasses')

dirs <- list.dirs(dataRootDir, full.names = T, recursive = F)
use <- 'Rhodes!wet!fur'
use <- 'Rhodes!wet!spr'

for (i in 1:length(dirs)) {
  
  fls <- list.files(paste0(dirs[i], '/',use, '/Chunks'))
  l <- length(fls)
  print(paste0(lim, ' : ', basename(dirs[i]), ' = ', l ))
  
}

