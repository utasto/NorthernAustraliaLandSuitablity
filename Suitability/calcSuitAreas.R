####################################################################################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 16:47:22 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Calculates the areas of suitabilities classes for each land use for project reporting
####################################################################################################

library(raster)
library(rgdal)
library(stringr)
library(readxl)
library(doParallel)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


srcDir <- paste0('C:/Projects/Roper/Suitability/MapsUTM')

fs <- list.files(srcDir, pattern = '.tif$', full.names = T, recursive = T)
statsDF <- data.frame(Suit=c(1,2,3,4,5,6))

no_cores <- detectCores()-3
no_cores <- 5 # reduced to control memeory usage - can't seem to get raster option to do this in parallel work
ptm <- proc.time()
cl<-makeCluster(no_cores)
registerDoParallel(cl)
dfl <- foreach(i=1:length(fs),  .packages=c('raster','rgdal', 'stringr')) %dopar% calcAreas(fs)
#dfl <- foreach(i=1:6,  .packages=c('raster','rgdal', 'stringr')) %dopar% calcAreas(fs)
##foreach(r=1:2,  .packages=c('raster','rgdal', 'Cubist')) %dopar% applyMapParallel( model, templateR, theStack, bs, outRasterDir)
stopCluster(cl)
proc.time() - ptm

e <- do.call(rbind, dfl)
e2 <- t(e)
e3 <- cbind(statsDF, e2)
luses <- str_replace(basename(fs), '_m.tif', '')
colnames(e3) <- c('Suitability', luses)
write.csv(e3, paste0('c:/Projects/Roper/Suitability/Suit_areas_Final.csv'), row.names = F)



calcAreas <- function(fs, statsDF){

   # for(i in 1:length(fs)){
    
      #for(i in 1:2){
  rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="",chunksize=1e+07,maxmemory=1e+07, overwrite=TRUE) # maxmemory = max no of cells to read into memory
  
      f <- fs[i]
      print(basename(f))
      pr1<- raster(f)
      #colnm <- str_split( basename(f), '\\$')[[1]][1]
      colnm <- str_replace( basename(f), '_Suit.tif', '')
      
      cellsize <- (res(pr1)[1] * res(pr1)[2]) / 10000
      statList <- numeric(length = 6)
      for(j in 1:5){
        v1<- which(pr1[]==j)
        areacalc <- length(v1)*cellsize
        statList[j] = areacalc
      }
      
      tot <- sum(statList[1:5])
      statList[6] <- tot
      #statsDF <- cbind(statsDF, statList)
      #colnames(statsDF)[i+1] <- colnm
      return (statList)
   # }
      
}

write.csv(statsDF, paste0('E:/Projects/NAWRA/Suitability/', catchment , '_Suit_areas.csv'))





