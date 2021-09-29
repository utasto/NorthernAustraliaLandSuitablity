a <- readRDS('Z:/Projects/NAWRA/Production/Limitations/Fitzroy/Moisture_availability_1.0/Cane!wet!fur/Chunks/subclasses_163.rds')

a<-1
b<-saveRDS(a, 'Z:/Projects/NAWRA/Productin/test.rds')

#filename <- 'Z:/Projects/NAWRA/Production/Limitations/Fitzroy/Microrelief/Macadamia!per!tri/Chunks/subclasses_160.rds'

#####  Delete if file is zero legth and or can't be read eg corrupted

fs <- list.files('Z:/Projects/NAWRA/Production/Limitations/Fitzroy/Microrelief', full.names = T, recursive = T)
fs <- list.files('Z:/Projects/NAWRA/Production/Limitations/Fitzroy/Irrigation_efficiency-high_rate_methods', full.names = T, recursive = T)
fs <- list.files('Z:/Projects/NAWRA/Production/Limitations/Fitzroy/Rockiness', full.names = T, recursive = T)
fs <- list.files('Z:/Projects/NAWRA/Production/Limitations/Fitzroy/Nutrient_balance', full.names = T, recursive = T)

length(fs)
for (i in 1:length(fs)){
  cat(i, ' ')
  filename <- fs[i]
  fi <- file.info(filename)
  if(!fi$isdir){
    #s<- file.info(fs[i])$size)
      if(fi$size == 0){
        print()
        unlink(filename)
      }
    
    out <- tryCatch(
      {
        a <- readRDS(filename)
      },
      error=function(cond) {
        print(paste0("Couldn't readfile - ", filename))
        unlink(filename)
        return(NA)
      },
      finally={
        #message("Done")
        }
    )    
    
  }
}

#############################################################################



###### Rename chunk files

rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
catchmentName <- 'Darwin'
attribute <- 'Slope'
replaceStr <- 'slope'
toStr <- 'r'

chunkPath <- paste0(rootDir, '/attributes/', attribute, '/', catchmentName, '/Chunks')

fs <- list.files(chunkPath, full.names = T, recursive = F)

for(i in 1:length(fs)){
  f<-fs[i]
  toname <- paste0(dirname(f), '/', str_replace(basename(f), replaceStr, toStr))
  file.rename(f, toname)
}


  




template <- raster(paste0(rootDir,'/Masks/template_', catchmentName, '.tif'))
bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))  
  
for (i in 1:bs$n){
  i=1
  mx <- ncol(template)
  my <- bs$nrows[i]
  mrows <- mx * my
  outm <- matrix(nrow = mrows, ncol = kernal)
  
  cells <- as.numeric(getValues(template,  row=bs$row[i], nrows=bs$nrows[i]))
  valsi <- which(!is.na(cells))
  valsnd <- which(is.na(cells))
  
}

