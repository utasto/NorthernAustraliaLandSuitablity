library(raster) 
library(rgdal)
library(doParallel)



###### generate DEM realisations  ###########
rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
catchmentName <- 'Fitzroy'


fmask <- raster( paste0(rootDir,'/Masks/1sec_template_', catchmentName, '.tif'))
#template <- raster(paste0(rootDir,'/Masks/template_', catchmentName, '.tif'))
#template <- raster(paste0(rootDir,'/Attributes/AWC150/Darwin/AWC150_Darwin.tif'))
#templateR <- raster(paste0(rootDir,'/Attributes/AWC150/Mitchell/AWC150_Mitchell.tif'))
templateR <- raster(paste0(rootDir,'/Attributes/AWC150/Fitzroy/AWC150_Fitzroy.tif'))
mas <- raster(paste0(rootDir,'/attributes/DEM/', catchmentName,  '.tif' ))
bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))

colNums <- seq(2, ncol(fmask), 3)
kernal = 9
set.seed(3000)


getDEMIts <- function(v){
  #3.6 is is sd from DEM report
  normDist <- rnorm(n=500, m=v, sd=3.6)
  return(normDist)
  
}


massageDEM <- function(template, bs, mas, colNums, kernal){
#for (i in 1:bs$n){
  #i=1
  
  mx <- ncol(template)
  my <- bs$nrows[i]
  mrows <- mx * my
  outm <- matrix(nrow = mrows, ncol = 1)
  
  cells <- as.numeric(getValues(template,  row=bs$row[i], nrows=bs$nrows[i]))
  cat(i, ' ')
  
  cnt=1
  for(j in 1:bs$nrows[i]){
    
    idx = bs$row[i] * 3  +  (j*3 - 2) - 2
    # cat(j, ' ')
    pixels <- getValuesFocal(mas, row=idx, nrows=1, ngb=3, names=F)
    p3 <- pixels[colNums,]
    
    outm[cnt:(cnt+nrow(p3)-1) ,1] <- (p3[,5] + 2) # datum difference offset
    cnt <- cnt + nrow(p3)
  }
  
  
  valsi <- which(!is.na(cells))
  valsnd <- which(is.na(cells))
  
  p3 <- outm[valsi,1]
  
  dps <- lapply(p3, getDEMIts )
  output <- matrix(unlist(dps), ncol = 500, byrow = TRUE)
  
  
  p4 <- cbind(valsi, output)
  
  fname <- paste0(rootDir,  '/attributes/DEM/', catchmentName, '/chunks/r_', i, '.rds')
  saveRDS(p4, fname)
  
}
  
  
  
  
  
  
  
  
  numcpus = detectCores()
  cl<-makeCluster(numcpus,outfile="")
  registerDoParallel(cl)
  foreach(i=1:bs$n, .packages=c( 'raster')) %dopar% massageDEM(templateR, bs, mas, colNums, kernal)
  stopCluster(cl)
  
  
  
  
  

