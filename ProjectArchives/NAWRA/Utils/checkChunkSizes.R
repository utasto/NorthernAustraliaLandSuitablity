library(stringr)

rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

getAttributeChunkSizes('Fitzroy')

getAttributeChunkSizes<-function(catchmentName){
  
  inDir <- paste0(rootDir, '/Attributes')
  # rootDir <- '//ternsoils/E/www/NAWRA'
  # #rootDir <- 'Q:/Projects/NAWRA/Production'
  # inDir <- paste0(rootDir, '/Attributes')
  
  attList <- list.dirs(path = inDir, full.names = T, recursive = F)
  
  for(i in 1:length(attList)){
    attName <- basename(attList[i])
    catDir <- paste0(attList[i], '/', catchmentName)
    
    #if(dir.exists(catDir)){
      rdsDir <- paste0(catDir, '/Chunks')
      rsds <- list.files(path = rdsDir, pattern='r_')
      print(paste0(catchmentName, ' - ' , attName,  ' - ' ,length(rsds)))
    #}
  }
}


###### Checks the number of valid cells for each chunk for each attribute
getChunkLines('Mitchell')
getChunkLines('Fitzroy')

getChunkLines<-function(catchmentName){
  
  inDir <- paste0(rootDir, '/Attributes')
  # rootDir <- '//ternsoils/E/www/NAWRA'
  # #rootDir <- 'Q:/Projects/NAWRA/Production'
  # inDir <- paste0(rootDir, '/Attributes')
  
  attList <- list.dirs(path = inDir, full.names = T, recursive = F)
  
  
  for(i in 1:length(attList)){
    attName <- basename(attList[i])
    catDir <- paste0(attList[i], '/', catchmentName)
    
    if(file.exists(catDir)){
        fname <- paste0(catDir, '/Chunks/r_1.rds')
        r <- readRDS(fname)
        print(paste0(dim(r)[1], ' - ', catchmentName, ' - ' , attName))
    }
    
  }
}




###### Checks the number of valid cells for each chunk for one known correct and one suspect attribute





catchmentName <- 'Fitzroy'
allmaps <- readAttributeFileMapppings()
maps <- allmaps[allmaps$Catchment == catchmentName & allmaps$ChunkName != '???',   ]
rl <- unique(maps$ChunkName)

toCheck <- paste0(rootDir, '/Attributes/',rl[k] ,'/',catchmentName , '/Chunks/r_')

templateR <-paste0(rootDir, '/Attributes/AWC150/', catchmentName,'/Chunks/r_')

for(k in 1:length(rl)){
  toCheck <-paste0(rootDir, '/Attributes/',rl[k] ,'/',catchmentName , '/Chunks/r_')
  print(toCheck)
  getallvaidCellSizes(templateR, toCheck, catchmentName)
}

templateR <-paste0(rootDir, '/Attributes/AWC150/Fitzroy/Chunks/r_')
toCheck <-paste0(rootDir, '/Attributes/SGG25_red6_May24r/Fitzroy/Chunks/r_')
toCheck <-paste0(rootDir, '/Attributes/SoilDepth_noC/Fitzroy/Chunks/r_')

getallvaidCellSizes(templateR, toCheck, catchmentName)

getallvaidCellSizes<-function( templatePath, toCheckPath, catchmentName){

  bs <- readRDS(paste0(rootDir, '/Masks/chunks_', catchmentName, '.rds'))

  for(i in 1:bs$n){
    #cat(i, ' ')
      fname <- paste0(templatePath, i, '.rds')
      r <- readRDS(fname)
      fname2 <- paste0(toCheckPath, i, '.rds')
      r2 <- readRDS(fname2)
      if(dim(r)[1] != dim(r2)[1]){
        print(paste0('Problem in chunk ', i, ' - ', dim(r)[1], ' - ', dim(r2)[1]))
      }
  }
  print(paste0("Done checking ", toCheckPath))
}






