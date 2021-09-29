
inDir <-  'V:/Projects/NAWRA/Production/Attributes/SoilDepthWAClasses_v7/Fitzroy/Chunks'
outDir <- 'V:/Projects/NAWRA/Production/Attributes/SoilDepthWAClasses_v7_Reclass/Fitzroy/Chunks/'

fList <- list.files(path = inDir, pattern = 'r_', full.names = T, recursive = F)
length(fList)

for(i in 1:length(fList)){
  
  df <- readRDS(fList[i])
  df[df==1] <- 1.5
  df[df==2] <- 1.0
  df[df==5] <- 0.25
  df[df==3] <- 0.5
  df[df==4] <- 5
  newfn <- paste0(outDir, basename(fList[i]))
  saveRDS(df, newfn)
}


d2 <- readRDS(newfn)
#### redo the summary files   ########################################################################
catchment <- 'Fitzroy'
att <- 'SoilDepthWAClasses_v7_Reclass'

suffix <-'_Uncert'
rasterDir <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/Chunks')
outRaster <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/', att, '_', catchment, '.tif')
print(paste0("creating - ", outRaster))
#bs <- blockSize

modelType<-'Classification'

for(i in 1:195)
{
  doRFSummaryRasterParallel(rootDir, catchment, att, modelType, templateR, bs, i)
}

doRFSummaryRasterParallel <- function(rootDir, catchment, att, modelType, templateR, bs, k){
  
  fname <- paste0(rootDir, '/Attributes/', att, '/', catchment, '/Chunks/r_', k, '.rds')
  print(fname)
  
  #if(file.exists(fname)){
  chunkVals <- readRDS(fname)
  inds <- chunkVals[,1]
  
  if(modelType == 'Classification'){
    
    sumData <- apply(chunkVals[,-1], 1, getCategoricalPredictionSummary)
    
  } else if(modelType == 'Regression'){
    
    sumData <- apply(chunkVals[,-1], 1, getContinuousPredictionSummary)
  }
  
  b <- do.call(rbind, sumData)
  sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
  
  vname <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/Chunks/v_', k, '.rds')
  print(paste0('vname = ' , vname))
  saveRDS(sumDataDF2, vname)
  #}
}



getModalValue <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

CV <- function(mean, sd){
  (sd/mean)*100
}

getCategoricalPredictionSummary <- function(v) {
  
  uniqv <- unique(v)
  if(length(uniqv) >1)
  {
    cnts <- tabulate(match(v, uniqv))
    modVal <- uniqv[which.max(cnts)]
    
    cntsDF <- matrix(c(uniqv,cnts),nrow=length(cnts))
    ordcntsDF <- cntsDF[order(-cnts),]
    v1 <- ordcntsDF[1,2]
    v2 <- ordcntsDF[2,2]
    ci <- v2/v1
  }
  else
  {
    modVal <- uniqv[1]
    ci <- 0
  }
  v = list(mode=modVal, confInd=ci)
}



getContinuousPredictionSummary <- function(v) {
  
  m <- mean(v)
  stdDev <- sd(v)
  
  (stdDev/m)*100
  
  ci <-(stdDev/m)*100
  v = list(pred=m, confInd=ci)
}










outRasterCI <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/', att, '_', catchment, suffix)

predR<-raster(templateR)
crs(predR) <- CRS("+proj=longlat +datum=WGS84")
predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
#outRasterCI <- paste0(rootDir, '/Attributes/', catchment, '/', att, '/', att , suffix)
predCI<-raster(templateR)
crs(predCI) <- CRS("+proj=longlat +datum=WGS84")
predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")

print(paste0("Creating Raster ", outRaster) ) 

for (i in 1:bs$n)
{
  bname = paste0(rasterDir, '/v_', i, '.rds')
  print(bname)
  
  csize<-bs$nrows[i] * ncol(templateR)
  b <- rep(NAvalue(templateR), csize )
  c <- rep(NAvalue(templateR), csize )
  if(file.exists(bname)){
    blockVals <- readRDS(bname)
    cat(paste(i, " ", sep=''))
    b[blockVals$inds] <- blockVals$modalVal
    c[blockVals$inds] <- blockVals$ci
    predR <- writeValues(predR, b, bs$row[i])
    predCI <- writeValues(predCI, c, bs$row[i])
  }
}

predR<-writeStop(predR)
predCI<-writeStop(predCI)
plot(predR, main=paste0(att, ' - ', catchment ))
plot(predCI, main=paste0(att, ' CI', ' - ', catchment ))
print("Done")