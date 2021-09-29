library(raster) 
library(rgdal)
library(ranger)
library(stringr)

###Run 1st to Run 2nd

makerasterFromTreePredictionChunks <- function(ProcessingDir = NULL, templateR = NULL, att = NULL, model = NULL , islog=F){
  
  rasterDir <- paste0(ProcessingDir, '/Maps/', att, '/Chunks')
  outRaster <- paste0(ProcessingDir, '/Maps/', att, '/', att, '.tif')
  print(paste0("creating - ", outRaster))
  
  fls <- list.files(rasterDir, pattern = 'cellSummaries')
  
  #templateR <- raster(paste0(rootDir, '/Templates/template.tif'))
  chk <-	getChunkInfo(chunkLines, nrow(templateR))
  modelType <- model$treetype
  
  suffix <-''
  if(modelType == 'Classification'){
    suffix <- '_CI.tif'
  }else{
    suffix <- '_CoV.tif'
  }
  
  
  outRasterCI <- paste0(ProcessingDir, '/Maps/',  att, '/', att, suffix)
  
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  predCI<-raster(templateR)
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  print(paste0("Creating Raster ", outRaster) ) 
  
  for (i in 1:length(fls))
  {
    bname = paste0(rasterDir, '/cellSummaries_', i, '.rds')
    
    
    csize<-chk$nlines[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    if(file.exists(bname)){
      print(bname)
      blockVals <- readRDS(bname)
      cat(paste(i, " ", sep=''))
      if(islog){
        exp(b[blockVals$inds] <- blockVals$modalVal)
        exp(c[blockVals$inds] <- blockVals$ci)
      }else{
        b[blockVals$inds] <- blockVals$modalVal
        c[blockVals$inds] <- blockVals$ci
      }
      
    }
      predR <- writeValues(predR, b, chk$starts[i])
      predCI <- writeValues(predCI, c, chk$starts[i])
    }
  
  
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  
  
  
  print("Done")
  
  return(list(valsRaster=outRaster, uncertRaster=outRasterCI))
}

###run 2nd


rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+10, overwrite=TRUE) # maxmemory = max no of cells to read into memory

args = commandArgs(trailingOnly=T)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  rootDir <- 'X:/work/3_Land_suitability/0_Working/Uta/Roper'
  rootDir <- 'N:/3_Land_suitability/0_Working/Uta/Roper'
  k=as.numeric(args[1])
  att = 'sal2_2w'
  modelFileName = 'sal2.mod.w2.rds'
  chunkLines = 20
  source('X:/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/RFUtils.R')
  
}else{
  rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
  k = as.numeric(args[1])
  att = 'sal2_2w'
  modelFileName = 'sal2.mod.2w.rds'
  chunkLines = 20
  source('/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/RFUtils.R')
}

# rootDir <- 'N:/3_Land_suitability/0_Working/Uta/Roper'
# modelFileName = 'drain.mod.3.rds'
# att <- 'perm3'
# att <- 'drain3'

model <- readRDS(paste0(rootDir, '/Models/', modelFileName))
templateR <- raster(paste0(rootDir, '/Templates/template.tif'))


pPath <- makerasterFromTreePredictionChunks(ProcessingDir = rootDir, templateR = templateR, att = att, model = model)

####look at raster
r <- raster('/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/Roper/Maps/Perm1/Perm3.tif')
plot(r, maxpixels=10000)


