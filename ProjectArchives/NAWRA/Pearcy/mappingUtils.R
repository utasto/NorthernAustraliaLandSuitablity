library(raster) 
library(rgdal)
library(rasterVis)

source('RandomForestUtils_V3.R')



doMapSuitabilities <- function(rootDir, landuse, catchment, templateR, bs){
  
  rasterDir <- paste0(rootDir, '/Suitability/', catchment)
  rdsDir <- paste0(rasterDir, '/', landuse, '/Chunks')
  
  use <- landuse
  outRaster <-   paste0(rasterDir, '/', use, '$', catchment, '$suitability_Cat.tif')
  outRasterCI <- paste0(rasterDir, '/', use, '$', catchment, '$suitability_Cat_CI.tif')
  outRasterCont <-   paste0(rasterDir, '/', use, '$', catchment, '$suitability_Cnt.tif')
  #outRasterCICont <- paste0(rasterDir, '/', use, '$', catchment, '$suitability_Cont_CoV.tif')
  
  print(paste0("creating - ", outRaster))
  
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="INT2S")
  
  predCI<-raster(templateR)
  predCI<-writeStart(predR,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  predRCont<-raster(templateR)
  predRCont<-writeStart(predRCont,filename=outRasterCont,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  # predRContCI<-raster(templateR)
  # predRContCI<-writeStart(predRContCI,filename=outRasterCICont,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  # 
  for (i in 1:bs$n)
  {
    bname = paste0(rdsDir, '/suit_', i, '.rds')
    csize<-bs$nrows[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    d <- rep(NAvalue(templateR), csize )
    #e <- rep(NAvalue(templateR), csize )
    
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
      #cat(paste(i, " ", sep=''))
      b[blockVals$ids] <- blockVals$modalVal
      c[blockVals$ids] <- blockVals$ci
      d[blockVals$ids] <- blockVals$cnt
      #e[blockVals$ids] <- blockVals$cov
      
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
      
      predRCont <- writeValues(predRCont, d, bs$row[i])
      #predRContCI <- writeValues(predRContCI, e, bs$row[i])
    }
  }
  
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  
  predRCont<-writeStop(predRCont)
  #predRContCI<-writeStop(predRContCI)
  
  print('')
  print(paste0('Finished generating ', basename(outRaster)))
  
  outPaths <- c(outRaster, outRasterCI, outRasterCont)
 
  return(outPaths)
}


plotSuits <- function(outPaths){
  
  #outPaths <-fnames
  Rsuit <- raster(outPaths[1])
  n <- c(1,2,3,4,5)
  cols <- c("darkgreen", "green", "yellow2", "orange", "brown")
  newrat <- data.frame("ID" = n, "Category" = n)
  colnames(newrat) <- c('ID', 'Category')
  levels(Rsuit) <- newrat
  levelplot(Rsuit, col.regions = cols)
  
  if(length(outPaths)>1){
    Rci <- raster(outPaths[2])
    plot(Rci)
  }
}





doMapSubClasses <- function(rootDir, catchment, limitation, landuse, templateR){
  
  usePath <- paste0(rootDir, '/Limitations/', catchment, '/', limitation, '/', landuse)
  outDir <- paste0(rootDir, '/Suitability/', catchment, '/', landuse)
  rasterDir <- paste0(usePath)
  use <- landuse
  
  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  
  outRaster <-   paste0(outDir, '/', use, '$', limitation, '$subclass.tif')
  outRasterCI <- paste0(outDir, '/', use, '$', limitation, '$subclass_CI.tif')
  
  print(paste0("creating - ", outRaster))
  
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="INT2S")
  
  predCI<-raster(templateR)
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  for (i in 1:bs$n)
  {
    # cat(i, ' ')
    paste0(usePath, '/Chunks')
    bname = paste0(rasterDir, '/Chunks/subclass_Suit_', i, '.rds')
    
    csize<-bs$nrows[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
      b[blockVals$inds] <- blockVals$modalVal
      c[blockVals$inds] <- blockVals$ci
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
    }
  }
  
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  #print(paste0('Finished generating ', basename(outRaster)))
  
  return(outRaster)
}




doMapAttributes <- function(rootDir, attribute, catchment, templateR, bs, continuousAtt=T){
  
  
  rasterDir <- paste0(rootDir, '/Attributes/', attribute, '/', catchment)
  
  outRaster <-   paste0(rasterDir, '/', attribute, '$', catchment, '.tif')
  outRasterCI <- paste0(rasterDir, '/', attribute, '$', catchment, '_CI.tif')
  
  print(paste0("creating - ", outRaster))
  
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  predCI<-raster(templateR)
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  for (i in 1:bs$n)
  {
    cat(i , ' ')
    
    bname = paste0(rasterDir, '/Chunks/r_', i, '.rds')
    
    csize<-bs$nrows[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
      
      if(continuousAtt) {
        sumData <- apply(blockVals[,-1], 1, getContinuousPredictionSummary)
      }else{
        sumData <- apply(blockVals[,-1], 1, getCategoricalPredictionSummary)
      }

      ids <- as.vector(blockVals[ , 1])
      sumvals <- do.call(rbind, sumData)
      outdf <- data.frame(ids, modalVal = unlist(sumvals[,1]), ci = unlist(sumvals[,2]))
      
      b[outdf$ids] <- outdf$modalVal
      c[outdf$ids] <- outdf$ci
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
    }
  }
  
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  
  print(paste0('Finished generating ', basename(outRaster)))
  
  return(outRaster)
}


















