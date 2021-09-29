# catchment climAtt
args = commandArgs(trailingOnly=TRUE)

#args = c('Darwin', 'Rain')

library(raster)
library(doParallel)

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/software/RStuff/myFunctions/RandomForestUtils_V3.R')

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory

rootDir = paste0('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production')
#templateR <- raster(paste0('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Masks/template_',catchment, '_geo.tif'))
#templateR <- raster(paste0(rootDir,'/Attributes/AWC150/Fitzroy/AWC150_Fitzroy.tif'))
templateR <- raster(paste0(rootDir,'/Attributes/AWC150/Mitchell/AWC150_Mitchell.tif'))


makeClimateChunks <- function(outDir, templateR, theStack, bs){  
  
  chunkRasterDir <- paste0(outDir)
  vname <- paste0(chunkRasterDir, '/v_' , k, '.rds')
  print(paste0('vname = ' , vname))
  
  ncells = bs$nrows[k] * ncol(templateR)
  theSeq = seq(ncells)
  covs = data.frame(theSeq)
  
  cells <- as.numeric(getValues(templateR,  row=bs$row[k], nrows=bs$nrows[k]))
  
  for (i in 1:nlayers(theStack)) 
  {
    rl = raster(theStack, layer=i)
    v <- as.numeric(getValues(rl, row=bs$row[k], nrows=bs$nrows[k]))
    covs[names(rl)] <- v    
    #print(i)
  }
  
  # valsi <- which(!is.na(covs[,2]))
  # valsnd <- which(is.na(covs[,2]))
  valsi <- which(!is.na(cells))
  valsnd <- which(is.na(cells))
  
  outM<- covs[valsi,-1]
  
  predsPlusCellID <- cbind(valsi, outM)
  m <- as.matrix(predsPlusCellID)
  
  
  bname = paste0(chunkRasterDir, '/r_' , k, '.rds')
  saveRDS(m, bname)
  
  sumData <- apply(outM[,-1], 1, getContinuousPredictionSummary)
  b <- do.call(rbind, sumData)
  sumDataDF2 <- data.frame(valsi, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
  
 
  saveRDS(sumDataDF2, vname)
  
}


 
  
 
  inDirRoot =  paste0('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Climate')
  
  numcpus = detectCores()


catchment <- args[1]
att <- args[2]
print(att)
print(catchment)

inDir =  paste0(inDirRoot, '/', catchment, '/RastersResamp/', att)

outDir = paste0(rootDir, '/Attributes/', att, '/', catchment,  '/Chunks')
dir.create(outDir, recursive = T)

bs <- readRDS(paste0(rootDir, '/Masks/chunks_', catchment, '.rds'))

files <- list.files(inDir, pattern = '\\.tif$', full.names = T)


theStack = stack()
for (i in 1:length(files)) 
{
  rname <- files[i]
  theStack<- addLayer(theStack,raster(paste0(rname)))
}
print(paste0('There are ', nlayers(theStack), ' in the stack'))


numcpus = detectCores()
cl<-makeCluster(numcpus,outfile="")
registerDoParallel(cl)
foreach(k=1:bs$n, .export= c("getContinuousPredictionSummary"), .packages=c('raster')) %dopar% makeClimateChunks(outDir, templateR, theStack, bs)
stopCluster(cl)


