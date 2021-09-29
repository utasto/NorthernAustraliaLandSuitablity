library(raster) 
library(rgdal)
library(doParallel)
library(readxl)
library(stringr)




getLimitationsPaths <- function(rootDir, mapFile){
  df <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/', mapFile), col_names = T))
  df
}

readSuitFramework <- function(rootDir, suitFile, sheet, classFile ){
  
 df <- read_excel(paste0(rootDir, '/SuitabilityFramework/', suitFile), col_names = F, sheet = sheet)
 #ncols <- dim(df)[2] -1
 #nrows <- dim(df)[1]
 startCol = 6
 ncols <- 131
 nrows <- 305
 

 
 cropNumbers <- as.numeric(unlist(as.list(df[2, 6:(ncols)])))
 cropTypes <- str_replace(trim(unlist(as.list(df[3, 6:(ncols)]))), ' ', '-')
 cropNames <- str_replace(trim(unlist(as.list(df[4, 6:(ncols)]))), ' ', '-')
 cropSeasons <- str_replace(trim(unlist(as.list(df[5, 6:(ncols)]))), ' ', '-')
 cropIrrigTypes <- str_replace(trim(unlist(as.list(df[6, 6:(ncols)]))), ' ', '-')
 
 fullNames <- paste(cropNames, cropSeasons, cropIrrigTypes, sep='!')
 
 suBclasses <- df[9:nrows, ]
 namesPre <- c('LIM_CODE',	'LIM_Name',	'LIM_VALUE_Desc_FGARA',	'LIM_VALUE',	'LIM_VALUE_Description')
 colnames(suBclasses) <- c(namesPre, fullNames)

 
 drops<-c(1,2,3,5)

 limClasses <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/', classFile), col_names = T))
 subClasses <- as.data.frame(suBclasses)
 suits <- merge(x = limClasses, y = subClasses[-drops] , by.x="LIM_VALUE", by.y = "LIM_VALUE", all.x=T, all.y=F)
 
 return (suits)
  
}

readAttributeFileMapppings <- function(rootDir='//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production', attMapFilename='LimitationsFileMappings.xlsx'){
  df <- read_excel(paste0(rootDir, '/SuitabilityFramework/', attMapFilename), col_names = T)
}

readSuitLanduses <- function(rootDir, suitFile, sheet, classFile ){
  
  df <- read_excel(paste0(rootDir, '/SuitabilityFramework/', suitFile), col_names = F, sheet = sheet)
  #ncols <- dim(df)[2] -1
  #nrows <- dim(df)[1]
  startCol = 6
  ncols <- 131
  nrows <- 305
  
  cropNumbers <- as.numeric(unlist(as.list(df[2, 6:(ncols)])))
  cropTypes <- str_replace(trim(unlist(as.list(df[3, 6:(ncols)]))), ' ', '-')
  #cropNames <- unlist(as.list(df[4, 6:(ncols)]))
  cropNames <- str_replace(trim(unlist(as.list(df[4, 6:(ncols)]))), ' ', '-')
  cropSeasons <- str_replace(trim(unlist(as.list(df[5, 6:(ncols)]))), ' ', '-')
  cropIrrigTypes <- str_replace(trim(unlist(as.list(df[6, 6:(ncols)]))), ' ', '-')
  
  fullNames <- paste(cropNames, cropSeasons, cropIrrigTypes, sep='!')
  
  return (fullNames)
}

readSuitLimitations <- function(rootDir,  classFile ){
  
  
  limClasses <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/', classFile), col_names = T))
 
  return ( unique(limClasses$LIM_Name ))
  
  
}












processSubClassesSerial <- function(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo, k){
  #print(lim)
  calcSubClasses(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, limInfo)
  doSubClassSummary(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)
}
doSubClassSummarySerial <- function(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo, k){
  
  for(j in 1: length(useList)){
    
    inDir <- paste0(rootDir, '/Limitations/', catchmentName, '/', lim, '/', useList[j],  '/Chunks')
    
    outfile <- paste0(inDir, '/subclass_Suit_', k, '.rds')
    #if(!file.exists(outfile)){
    
    infile <- paste0(inDir,  '/subclasses_', k, '.rds')
    print(infile) 
    fOut <- readRDS(infile)
    
    #### do the subclass suitability summary
    inds <- fOut[,1]
    sumData <- apply(fOut[,-1], 1, getCategoricalPredictionSummary)
    b <- do.call(rbind, sumData)
    sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
    
    vname <- paste0(inDir, '/subclass_Suit_', k, '.rds')
    saveRDS(sumDataDF2, vname)
    # }
  }
}

processSubClassesParallel <- function(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo){
  #print(lim)
  calcSubClasses(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, limInfo)
  doSubClassSummary(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)
}

doSubClassSummary <- function(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo){

    for(j in 1: length(useList)){
      
        inDir <- paste0(rootDir, '/Limitations/', catchmentName, '/', lim, '/', useList[j],  '/Chunks')
        
          outfile <- paste0(inDir, '/subclass_Suit_', k, '.rds')
          #if(!file.exists(outfile)){
            
              infile <- paste0(inDir,  '/subclasses_', k, '.rds')
              print(infile) 
              fOut <- readRDS(infile)
        
             #### do the subclass suitability summary
              inds <- fOut[,1]
              sumData <- apply(fOut[,-1], 1, getCategoricalPredictionSummary)
              b <- do.call(rbind, sumData)
              sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
               
              vname <- paste0(inDir, '/subclass_Suit_', k, '.rds')
            saveRDS(sumDataDF2, vname)
       # }
    }
}


calcSubClasses <- function(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, limInfo){
  
  limMappping1 <- limRastersList[limRastersList$Limitation == lim & limRastersList$Catchment == catchmentName,]

  
  
  fname <- paste0(rootDir, '/Attributes/', limMappping1[1,4], '/', catchmentName, '/Chunks/r_', k, '.rds')
  limVals1 <- readRDS(fname)
  
  cellids <-  limVals1[,1]
  warr1 <- limVals1[,-1]
  
  
  if(limInfo$NumQuals[1] == 2)
  {
    fname2 <- paste0(rootDir, '/Attributes/', limMappping1[2,4], '/', catchmentName, '/Chunks/r_', k, '.rds')
    limVals2 <- readRDS(fname2)
    warr2 <- limVals2[,-1]
    
    #Hack to deal with possible negative values in slope surface
    if( lim == "Erosion"){
      warr2[warr2<0]<-0
    }
  }
  if(limInfo$NumQuals[1] == 3)
  {
    fname2 <- paste0(rootDir, '/Attributes/', limMappping1[2,4], '/', catchmentName, '/Chunks/r_', k, '.rds')
    limVals2 <- readRDS(fname2)
    warr2 <- limVals2[,-1]
    
    fname3 <- paste0(rootDir, '/Attributes/', limMappping1[3,4], '/', catchmentName, '/Chunks/r_', k, '.rds')
    limVals3 <- readRDS(fname2)
    warr3 <- limVals3[,-1]
  }
  
  for(j in 1: length(useList)){
    
    
    landuse <- useList[j]
    outDir <- paste0(rootDir, '/Limitations/', catchmentName, '/', lim, '/', landuse,  '/Chunks')
    outfile <- paste0(outDir, '/subclasses_', k, '.rds')
    
   # if(!file.exists(outfile)){
    
    outMatrix = matrix( -1, nrow=nrow(limVals1), ncol(limVals1)-1) 
    
    alllims <- data.frame(limInfo$LIM_VALUE,limInfo$LIM_Name, limInfo[,5:14], limInfo[landuse])
    colnames(alllims)[13] <- landuse
    validInds <- which(!is.na(alllims[landuse]))
    lims <- alllims[validInds,] 
    
    
    if(nrow(lims) > 0){
      
      for (i in 1:nrow(lims))
      {
        
        if( lims$limInfo.LIM_Name[1] == "Climate-annual_rainfall"){
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-heat_dry_season_and_perennial"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-heat_wet_season_and_perennial"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-frost"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-temp_variation"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Flooding"){
          stop("Not implemented yet!")
        }
        else if( lims$limInfo.LIM_Name[1] == "Erosion"){
    
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
          
        }
        else if( lims$limInfo.LIM_Name[1] == "Wetness"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_1.5"){
          
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_1.0"){
          
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
          
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_0.6"){
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Nutrient_balance"){
          
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Soil_depth"){
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Rockiness"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Microrelief"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Irrigation_efficiency"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Irrigation_efficiency-high_rate_methods"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Salinity_surface"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Acid_Sulfate_Soil_Potential"){
          
          #stop("Not implemented yet!") ###############################################################
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_RAINFED_CROPPING_10"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 < lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_RAINFED_CROPPING_15"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 < lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_RAINFED_CROPPING_06"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 < lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        
        else if( lims$limInfo.LIM_Name[1] == "Surface_Condition"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i] & warr3 >= lims$QualVal3_min[i] & warr3 <= lims$QualVal3_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Surface_Texture"){
          
          if(i==5){ # special case for this particular rule - Pa5
            
            fname4 <- paste0(rootDir, '/Attributes/', limMappping1[4,4], '/', catchmentName, '/Chunks/r_', k, '.rds')
            limVals4 <- readRDS(fname4)
            warr4 <- limVals4[,-1]
            
            fname5 <- paste0(rootDir, '/Attributes/', limMappping1[5,4], '/', catchmentName, '/Chunks/r_', k, '.rds')
            limVals5 <- readRDS(fname5)
            warr5 <- limVals5[,-1]
            
            inds <- which(warr4 >= lims$QualVal1_min[i] & warr4 <= lims$QualVal1_max[i] & warr5 >= lims$QualVal2_min[i] & warr5 <= lims$QualVal3_max[i])
            outMatrix[inds] <- as.integer(lims[landuse][i,])
            print('Pa5')
            
          }else{
              inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal3_max[i] & warr3 >= lims$QualVal3_min[i] & warr3 <= lims$QualVal3_max[i])
              outMatrix[inds] <- as.integer(lims[landuse][i,])
          }
        }
        else if( lims$limInfo.LIM_Name[1] == "Surface_Infiltration"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i] )
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        
        else{
          
          stop("No limitation processing implemented yet for this limitation!")
        }
      #}
    
      
  }
     
      fOut <- cbind(cellids, outMatrix)
      saveRDS(fOut, paste0(outDir, '/subclasses_', k, '.rds'))
      print(paste0(outDir, '/subclasses_', k, '.rds'))
      #return(fOut)
      
    }
  }
  #}
}







calculateSuitabilities <- function(rootDir, landuse, catchmentName, currentLims, outDir){
  
  
  numcpus<-detectCores()
  
  #numcpus<-1
  
  print("Starting multi threading...")
  cl<-makeCluster(numcpus)
  registerDoParallel(cl)
  #foreach(k=1:bs$n, .export= c('getCategoricalPredictionSummary', 'getContinuousPredictionSummary', 'calculateOverallSuitabilites')) %dopar%  calculateOverallSuitabilites(rootDir, landuse, catchmentName, currentLims, outDir)
  foreach(k=1:bs$n, .export= c('getVectorStats', 'getMaxLimStats', 'CV','getCategoricalPredictionSummary', 'calculateOverallSuitabilitesModal')) %dopar%  calculateOverallSuitabilitesModal(rootDir, landuse, catchmentName, currentLims, outDir)
  
  stopCluster(cl)
  
  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  templateR <- raster(paste0(rootDir, '/Masks/template_' , catchmentName,'_geo.tif'))
  
  doMapSuitabilities(rootDir, landuse, catchmentName, templateR, bs)
  
  limsInfoFile <- paste0(rootDir, '/Suitability/', catchmentName, '/', landuse, '$', catchmentName, '$suitability.csv')
  write.csv(l, limsInfoFile)
  
}


calculateOverallSuitabilites <- function (rootDir, landuse, catchmentName, currentLims, outDir){
  
  
  limsDir1st <- paste0(rootDir, '/Limitations/', catchmentName, '/', currentLims[1,1])
  rdsPath <- paste0(limsDir1st, '/', landuse, '/Chunks/subclasses_', k, '.rds')
  rdsTemplate <-readRDS(rdsPath)
  theSeq = seq(nrow(rdsTemplate))
  
  
  suitArray <- array(-1, dim=c(nrow(rdsTemplate), ncol(rdsTemplate)-1, nrow(currentLims)))
  
  for(i in 1:nrow(currentLims)){
    
    rdsPath <- paste0(rootDir, '/Limitations/', catchmentName, '/', currentLims[i,1], '/', landuse, '/Chunks/subclasses_', k, '.rds')
    rds <- readRDS(rdsPath)
    suitArray[, , i] <- rds[,-1] 
  }
  
  
  overallSuitMatrix <- matrix(-1, nrow = nrow(rds), ncol = ncol(rds)-1)
  
  
  for(j in 1:dim(suitArray)[2]){
    m <- suitArray[, j,]
    suitvals <- apply(m,1,max)
    overallSuitMatrix[,j] <- suitvals
  }
  
  
  
  sumDataCat <- apply(overallSuitMatrix, 1, getCategoricalPredictionSummary)
  sumDataMean <- apply(overallSuitMatrix, 1, getContinuousPredictionSummary)
  
  
  ids <- as.vector(rdsTemplate[ , 1])
  
  catVals <- do.call(rbind, sumDataCat)
  conVals <- do.call(rbind, sumDataMean)
  outdf <- data.frame(ids, modalVal = unlist(catVals[,1]), ci = unlist(catVals[,2]), meanVal = unlist(conVals[,1]), cov = unlist(conVals[,2]))
  
  
  vname <- paste0(outDir, '/suit_', k, '.rds')
  print(paste0('vname = ' , vname))
  saveRDS(outdf, vname)
  
}

#calculateOverallSuitabilitesModal(rootDir, landuse, catchmentName, currentLims, outDir)

calculateOverallSuitabilitesModal <- function (rootDir, landuse, catchmentName, currentLims, outDir){
  
  
  limsDir1st <- paste0(rootDir, '/Limitations/', catchmentName, '/', currentLims[1,1])
  rdsPath <- paste0(limsDir1st, '/', landuse, '/Chunks/subclasses_', k, '.rds')
  rdsTemplate <-readRDS(rdsPath)
  theSeq = seq(nrow(rdsTemplate))

  suitdf <- rdsTemplate[,1]
  cisdf <- rdsTemplate[,1]
  for(i in 1:nrow(currentLims)){
    
    rdsPath <- paste0(rootDir, '/Limitations/', catchmentName, '/', currentLims[i,1], '/', landuse, '/Chunks/subclasses_', k, '.rds')
    rds <- readRDS(rdsPath)
  
    stats <- apply(rds[, -1], 1,  getVectorStats)
    catVals <- do.call(rbind, stats)
    subs <- unlist(catVals[,1])
    cis <- unlist(catVals[,2])
    suitdf<- cbind(suitdf, subs)
    cisdf<- cbind(cisdf, cis)
  }
  
  colnames(suitdf) <- c('cid',  as.character(currentLims[,1]))
  colnames(cisdf) <- c('cid',  as.character(currentLims[,1]))

  A <- suitdf[,-1]
  B <- cisdf[,-1]
  dfIn <- as.data.frame(cbind(A,B)) ### this is a hack to send two dfs to the apply function - seems to work ok though even if I do say so myself
  
  suitvals <- apply(dfIn,1,getMaxLimStats, colsr=ncol(A))
  outVals <- do.call(rbind, suitvals)
  ids <- as.vector(rdsTemplate[ , 1])
  outdf <- data.frame(ids, modalVal = unlist(outVals[,1]), ci = unlist(outVals[,2]), cnt = unlist(outVals[,3]))
  
  
  vname <- paste0(outDir, '/suit_', k, '.rds')
  print(paste0('vname = ' , vname))
  saveRDS(outdf, vname)
  
}







getAvailableLimitations<-function(catchmentName, landuse, suitFramework){
  
  cats <- c('Darwin', 'Fitzroy', 'Mitchell')
  cnts <- c(198, 195, 199)
  #cnts <- c(79, 300, 400)
  rdsCnts <- setNames(as.list(cnts), cats)
  
  requiredLims <- getRequiredLimitations(catchmentName, landuse, suitFramework)
  avail <- rep(F, length(requiredLims))
  correctCnt <- rep(F, length(requiredLims))
  requiredLimsDF <- data.frame(requiredLims, avail,correctCnt)
  
  rdscnt <- rdsCnts[catchmentName][[1]][1]
  
  inDir <- paste0(rootDir, '/Limitations/', catchmentName)

  limPathList <- list.dirs(path = inDir, full.names = T, recursive = F)

  for(i in 1:length(limPathList)){

    limName <- basename(limPathList[i])
    ind <- which(requiredLimsDF$requiredLims == limName)
    if (length(ind) > 0){
      #print(limName)
      requiredLimsDF$avail[ind[[1]]] <- T

      # limDir <- paste0(limPathList[i], '/', landuse, '/Chunks')
      # rsds <- list.files(path = limDir, pattern='subclasses_')
      # 
      # if( length(rsds) == rdscnt){
      #   requiredLimsDF$correctCnt[ind[[1]]] <- T
      # }

    }
    #}
  }
  return(requiredLimsDF)
}

getRequiredLimitations<-function(catchmentName, landuse, suitFramework){
  
  #print(as.character(landuse))
  luse<- as.character(landuse)
  #cid <- grep(landuse, colnames(suitFramework))
  alllims <- data.frame(suitFramework$LIM_VALUE,suitFramework$LIM_Name, suitFramework[,5:14], suitFramework[luse])
  colnames(alllims)[13] <- luse
  notNALims <- alllims[!is.na(alllims[luse]),]
  usedLims <- as.character(unique(notNALims$suitFramework.LIM_Name))
  return (usedLims)
  
}






