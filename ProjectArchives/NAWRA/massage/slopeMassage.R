library(raster) 
library(rgdal)
library(doParallel)


# rootDir = 'd:/temp/slope'
# #catchmentName <- 'Darwin'
# #catchmentName <- 'Mitchell'
# catchmentName <- 'Fitzroy'
# 
# template <- raster(paste0(rootDir,'/Masks/template_', catchmentName, '.tif'))
# fmask <- disaggregate(template,3)
# crs(fmask)<- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
# writeRaster(fmask, paste0(rootDir,'/Masks/1sec_template_', catchmentName, '.tif'))
# 
# r <- raster('d:/temp/slope/slopepct1s.tif')
# outfile <- paste0('d:/temp/slope/', catchmentName,  '.tif' )
# cr <- crop(r, template)
# crs(cr)<- crs(template)
# crs(fmask)<- crs(template)
# mas <- mask(cr, fmask, filename=outfile, overwrite=T)
# 
# 


getSlopeIts <- function(v){
  mn <-mean(na.omit(v))
  std <-sd(na.omit(v))
  normDist <- rnorm(n=500, m=mn, sd=std)
  return(normDist)
  
}



###### generate slope realisations  ###########
rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
catchmentName <- 'Darwin'
#catchmentName <- 'Mitchell'
#catchmentName <- 'Fitzroy'

fmask <- raster( paste0(rootDir,'/Masks/1sec_template_', catchmentName, '.tif'))
#template <- raster(paste0(rootDir, '/Attributes/AWC150/Darwin/AWC150_Darwin.tif'))
#templateR <- raster(paste0(rootDir,'/Attributes/AWC150/Mitchell/AWC150_Mitchell.tif'))
templateR <- raster(paste0(rootDir,'/Attributes/AWC150/Darwin/AWC150_Darwin.tif'))
mas <- raster(paste0(paste0(rootDir, '/attributes/slope/', catchmentName,  '.tif' )))
bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))


colNums <- seq(2, ncol(fmask), 3)
kernal = 9
set.seed(3000)

massageSlope <- function(templateR, bs, mas, colNums, kernal){
#for (i in 1:bs$n){
 
  mx <- ncol(templateR)
  my <- bs$nrows[i]
  mrows <- mx * my
  outm <- matrix(nrow = mrows, ncol = kernal)
  
  cells <- as.numeric(getValues(templateR,  row=bs$row[i], nrows=bs$nrows[i]))
  cat(i, ' ')
  
  cnt=1
  for(j in 1:bs$nrows[i]){
    #j=1
    idx = bs$row[i] * 3  +  (j*3 - 2) - 2
    # cat(j, ' ')
    pixels <- getValuesFocal(mas, row=idx, nrows=1, ngb=3, names=F)
    p3 <- pixels[colNums,]
    
    outm[cnt:(cnt+nrow(p3)-1) ,] <- p3
    cnt <- cnt + nrow(p3)
  }
  
  
  valsi <- which(!is.na(cells))
  valsnd <- which(is.na(cells))
  
  p3 <- outm[valsi,]
  
  dps <- apply(p3, 1, getSlopeIts )
  dpst <- t(dps)
  
  p4 <- cbind(valsi, dpst)
  fname <- paste0(rootDir,  '/attributes/Slope/', catchmentName, '/chunks/r_', i, '.rds')
  
  saveRDS(p4, fname)
  
}



numcpus = detectCores()-3
cl<-makeCluster(numcpus,outfile="")
registerDoParallel(cl)
foreach(i=1:bs$n, .packages=c( 'raster')) %dopar% massageSlope(templateR, bs, mas, colNums, kernal)
stopCluster(cl)






