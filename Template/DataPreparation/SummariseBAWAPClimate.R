
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:57:38 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Summarise the daily BAWAP data for to produce the various required climate limitation rasters
#################################

library(raster)
library(stringr)
library(rgdal)

library(rgdal)
library(raster)
library(ncdf4)
library(doParallel)
library(doSNOW)
library(stringr)


#rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/APSIM/BAWAP/Silo'
rootDir <- 'd:/Projects/Climate/BAWAP'
sumRastersPath <- 'd:/Projects/Climate/BAWAP/Summaries/SummaryRasters'
#sumRastersPath <- 'd:/temp'

att <- 'rain'
stat <- 'sum'

#outPath <- paste0(rootDir, '/Summaries/', att, '_', stat)
outPath <- 'd:/temp/demo'
if(!dir.exists(outPath))(dir.create(outPath, recursive = T))


dirs <- list.dirs(paste0(rootDir, '/', att), recursive = F)



#####   Annual Rainfall Summaries
numcpus <- 7
cl <- makeSOCKcluster(numcpus)
registerDoSNOW(cl)
pb <- txtProgressBar(initial=1, min=1, max=length(dirs), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
result <- foreach(i=1:length(dirs), .packages=c('raster'), .options.snow=opts)  %dopar% { sumAnnual(dirs, outPath, att, stat)  }
close(pb)
stopCluster(cl)
gc()


sumAnnual <- function(dirs, outPath, att, stat){
  
  dPath <- dirs[i]
  fls <- list.files(dirs[i], pattern = '*.nc', full.names = T)
  
  # for (j in 1:length(fls)) {
  #   bits <- str_split(fls[j])
  #   
  # }
  stk <- stack(fls)
  sr <- sum(stk)
  yr <- basename(dPath)
  rPath <- paste0(outPath,'/', att,'_', stat, '_', yr, '.tif' )
  writeRaster(sr, rPath)
  
}


fls <- list.files(outPath, full.names = T)
stk <- stack(fls)
mwr <- mean(stk)
plot(mwr)
writeRaster(mwr, paste0(sumRastersPath, '/', att,'_', stat, '_Avg.tif') )


#####   No days temps below 2 degrees

att <- 'tmin'
stat <- 'LessThan2Count'
outPath <- paste0(rootDir, '/Summaries/', att, '_', stat)

if(!dir.exists(outPath))(dir.create(outPath, recursive = T))


dirs <- list.dirs(paste0(rootDir, '/', att), recursive = F)

numcpus <- 7
cl <- makeSOCKcluster(numcpus)
registerDoSNOW(cl)
pb <- txtProgressBar(initial=1, min=1, max=length(dirs), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
result <- foreach(i=1:length(dirs), .packages=c('raster'), .options.snow=opts)  %dopar% { countBelow(dirs, outPath, att, stat)  }
close(pb)
stopCluster(cl)
gc()


countBelow <- function(dirs, outPath, att, stat){
  
  dPath <- dirs[i]
  fls <- list.files(dirs[i], pattern = '*.nc', full.names = T)
  stk <- stack(fls)
  frost.days <- calc(stk, fun=function(x, na.rm = T) sum(x<2))
  yr <- basename(dPath)
  rPath <- paste0(outPath,'/', att,'_', stat, '_', yr, '.tif' )
  writeRaster(frost.days, rPath)
  
}

fls <- list.files(outPath, full.names = T)
stk <- stack(fls)
mwr <- mean(stk)
plot(mwr)
writeRaster(mwr, paste0(sumRastersPath, '/', att,'_', stat, '_Avg.tif') )




######   Number of hot days wetseason

wetMonths = c(11,12,1,2,3,4)
dryMonths = c(5,6,7,8,9,10)

att <- 'tmax'
# stat <- 'WetGreaterThan35'
# reqMonths = wetMonths


te = 40
#stat <- paste0('DryGreaterThan', 35)
#stat <- paste0('DryGreaterThan', 38)
#stat <- paste0('DryGreaterThan', 40)
stat <- paste0('WetGreaterThan', 40)
#reqMonths <- dryMonths
reqMonths <- wetMonths


outPath <- paste0(rootDir, '/Summaries/', att, '_', stat)
if(!dir.exists(outPath))(dir.create(outPath, recursive = T))


dirs <- list.dirs(paste0(rootDir, '/', att), recursive = F)

numcpus <- 8
cl <- makeSOCKcluster(numcpus)
registerDoSNOW(cl)
pb <- txtProgressBar(initial=1, min=1, max=length(dirs), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
result <- foreach(i=1:length(dirs), .packages=c('raster', 'stringr'), .options.snow=opts)  %dopar% { countAbove(dirs, outPath, att, stat, te)  }
close(pb)
stopCluster(cl)
gc()


countAbove <- function(dirs, outPath, att, stat, te){
  
  dPath <- dirs[i]
  fls <- list.files(dirs[i], pattern = '*.nc', full.names = T)
  stk = stack()
  for (j in 1:length(fls)) {
    
    fname <- fls[j]
    print(fname)
    bits <- str_split(fname, '-')
    dt <- bits[[1]][4]
    month = as.numeric(str_sub(dt, 5 ,6))
    if(month %in% reqMonths){
      print(j)
      stk <- addLayer(stk, raster(fname))
    }
  }

  #hot.days <- calc(stk, fun=function(x, na.rm = T) sum(x>35))
  #hot.days <- calc(stk, fun=function(x, na.rm = T) sum(x>38))
  hot.days <- calc(stk, fun=function(x, na.rm = T) sum(x>te))
  yr <- basename(dPath)
  rPath <- paste0(outPath,'/', att,'_', stat, '_', yr, '.tif' )
  writeRaster(hot.days, rPath)
  
}

fls <- list.files(outPath, full.names = T)
stk <- stack(fls)
mwr <- mean(stk)
plot(mwr)
writeRaster(mwr, paste0(sumRastersPath, '/', att,'_', stat, '_Avg.tif') )



#####   Temperature variation

att <- 'tmin'
stat <- 'MonthsLessThan15'
outPath <- paste0(rootDir, '/Summaries/', att, '_', stat)

if(!dir.exists(outPath))(dir.create(outPath, recursive = T))


dirs <- list.dirs(paste0(rootDir, '/', att), recursive = F)

numcpus <- 7
cl <- makeSOCKcluster(numcpus)
registerDoSNOW(cl)
pb <- txtProgressBar(initial=1, min=1, max=length(dirs), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
result <- foreach(i=1:length(dirs), .packages=c('raster', 'stringr'), .options.snow=opts)  %dopar% { countMonthsLessThan15(dirs, outPath, att, stat)  }
close(pb)
stopCluster(cl)
gc()


countMonthsLessThan15 <- function(dirs, outPath, att, stat){
  
  dPath <- dirs[i]
  year <- basename(dPath)
  fls <- list.files(dirs[i], pattern = '*.nc', full.names = T)
  
  mstk <- stack()
  for (k in 1:12) {
    month = str_pad(k, 2, pad = "0")
    flsmon <- fls[grepl(paste0(year, month), fls)]
    stk <- stack(flsmon)
    cool.days <- calc(stk, fun=function(x, na.rm = T) mean(x))
    names(cool.days) = paste0(month)
    mstk <- addLayer( mstk, cool.days)
  }
  
  numMonths <- calc(mstk, fun=function(x, na.rm = T) sum(x<15))

  rPath <- paste0(outPath,'/', att,'_', stat, '_', year, '.tif' )
  writeRaster(numMonths, rPath)
  
}

fls <- list.files(outPath, full.names = T)
stk <- stack(fls)
mwr <- mean(stk)
plot(mwr)
writeRaster(mwr, paste0(sumRastersPath, '/', att,'_', stat, '_Avg.tif') )





#### Crop to project area

template <- raster('C:/Projects/Roper/Suitability/LimitationRasters/Ahor.mod.2.tif' )

attR <-  raster('C:/Projects/Climate/BAWAP/Summaries/SummaryRasters/rain_sum_Avg.tif')
r <- resample(attR, template, method='ngb')
writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/Rain.tif')
plot(r)

attR <-  raster('C:/Projects/Climate/BAWAP/Summaries/SummaryRasters/tmax_DryGreaterThan35_Avg.tif')
r <- resample(attR, template, method='ngb')
writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/tmax_DryGreaterThan35_Avg.tif')

attR <-  raster('C:/Projects/Climate/BAWAP/Summaries/SummaryRasters/tmax_WetGreaterThan35_Avg.tif')
r <- resample(attR, template, method='ngb')
writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/tmax_WetGreaterThan35_Avg.tif')

writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/tmax_WetGreaterThan35_Avg.tif')


attR <-  raster('C:/Projects/Climate/BAWAP/Summaries/SummaryRasters/tmin_LessThan2Count_Avg.tif')
r <- resample(attR, template, method='ngb')
writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/tmin_LessThan2Count_Avg.tif')

attR <-  raster('C:/Projects/Climate/BAWAP/Summaries/SummaryRasters/tmin_MonthsLessThan15_Avg.tif')
r <- resample(attR, template, method='ngb')
writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/tmin_MonthsLessThan15_Avg.tif')





attR <-  raster('d:/Projects/Climate/BAWAP/Summaries/SummaryRasters/tmax_DryGreaterThan38_Avg.tif')
r <- resample(attR, template, method='ngb')
writeRaster(r, 'C:/Projects/Roper/Suitability/LimitationRasters/tmax_WetGreaterThan35_Avg.tif')

attR <-  raster('C:/Projects/Climate/BAWAP/Summaries/SummaryRasters/tmax_WetGreaterThan35_Avg.tif')
r <- resample(attR, template, method='ngb')








