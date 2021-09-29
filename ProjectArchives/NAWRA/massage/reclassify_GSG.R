
inDir <- 'V:/Projects/NAWRA/Production/Attributes/SGGQNT_v8/Mitchell/Chunks'
outDir <- 'V:/Projects/NAWRA/Production/Attributes/SGGQNT_v8_Reclass/Mitchell/Chunks/'

fList <- list.files(path = inDir, pattern = 'r_', full.names = T, recursive = F)
length(fList)

for(i in 1:length(fList)){
  
  df <- readRDS(fList[i])
  df[df==81 | df==82 | df==83] <- 8
  newfn <- paste0(outDir, basename(fList[i]))
  saveRDS(df, newfn)
}



catchment <- 'Mitchell'
att <- 'SGGQNT_v8_Reclass'

suffix <-'_Uncert'
rasterDir <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/Chunks')
outRaster <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/', att, '_', catchment, '.tif')
print(paste0("creating - ", outRaster))
#bs <- blockSize

modelType<-'Classification'

for(i in 1:198)
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





