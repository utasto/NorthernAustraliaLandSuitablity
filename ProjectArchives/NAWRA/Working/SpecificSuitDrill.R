
library(readxl)
library(raster)
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
catchmentName <- 'Fitzroy'
catchmentName <- 'Darwin'

lat <- -18.4
lon <- 123.7317

lon <- 124.505
lat <- -17.84417

lon <- 125.7808
lat <- -18.6075

lat <- -13.3380 
lon <- 131.7239



landuse <- 'Maize!wet!fur'
landuse <- 'Asian-veg!dry!tri'
landuse <- 'Banana!per!tri'
landuse <- 'Cane!wet-long!fur'
landuse <- 'Cotton!dry!fur'
landuse <- 'Cucumber!dry!tri'
landuse <- 

theLim <- 'Erosion'
theLim <- 'Wetness'
theLim <- 'Surface_Condition'
theLim <- 'Moisture_availability_1.0'

  AllUses <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/PriorityLanduses.xlsx'), sheet='PriorityUses', col_names = T))

  
  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  templateR <- raster(paste0(rootDir, '/Masks/template_' , catchmentName,'.tif'))
  cellFromXY(templateR, cbind(c(lon), c(lat)))
  col <- colFromX(templateR, lon)
  row <- rowFromY(templateR, lat)
  chunk <- ( (row-1) %/% bs$nrows[1]) + 1
  chunkline <- ((row-1) %% bs$nrows[1]) + 1
  chunckCellInd <- ((chunkline-1) * ncol(templateR)) + col
  
  
  thisUse <- landuse
  ######################   Pull out single  limitation  ####################
  # Modal Subclasses
  rdsm <- readRDS(paste0(rootDir, '/Limitations/' , catchmentName, '/', theLim, '/', thisUse, '/Chunks/subclass_Suit_', chunk, '.rds'))
  rowid <- which(rdsm[,1] == chunckCellInd)
  valsm<- rdsm[rowid,]
  valsm
  
  
  
  lim <- theLim
  suitfile <- paste0(catchmentName, '_Suitability_Framework.xlsx')
  sheet <- 'SUBCLASSES-2-M-linked'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  limRastersList <- getLimitationsPaths(rootDir = rootDir, mapFile = 'LimitationsFileMappings.xlsx')
  

  
  bs  <- readRDS(paste0(rootDir, '/Masks/chunks_' , catchmentName,'.rds'))
  limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]
  

  
  
  limMappping1 <- limRastersList[limRastersList$Limitation == lim & limRastersList$Catchment == catchmentName,]
  limVals1 <- readRDS(paste0(rootDir, '/Attributes/', limMappping1[1,4], '/', catchmentName, '/Chunks/r_', chunk, '.rds'))
  cellids <-  limVals1[,1]
  warr1 <- limVals1[,-1]
  
  rowid <- which(cellids == chunckCellInd)
  mean(warr1[rowid,])
  hist(warr1[rowid,])
 
  
  if(limInfo$NumQuals[1] == 2)
  {
    fname2 <- paste0(rootDir, '/Attributes/', limMappping1[2,4], '/', catchmentName, '/Chunks/r_', chunk, '.rds')
    limVals2 <- readRDS(fname2)
    warr2 <- limVals2[,-1]
    
    #Hack to deal with possible negative values in slope surface
    if( lim == "Erosion"){
      warr2[warr2<0]<-0
    }
  }
  
  hist(warr2[rowid,])
  
  fname3 <- paste0(rootDir, '/Attributes/', limMappping1[3,4], '/', catchmentName, '/Chunks/r_', chunk, '.rds')
  limVals3 <- readRDS(fname2)
  warr3 <- limVals3[,-1]
  
  
  outMatrix = matrix( -1, nrow=nrow(limVals1), ncol(limVals1)-1) 
  
  
 
  alllims <- data.frame(limInfo$LIM_VALUE,limInfo$LIM_Name, limInfo[,5:14], limInfo[landuse])
  colnames(alllims)[13] <- landuse
  validInds <- which(!is.na(alllims[landuse]))
  lims <- alllims[validInds,] 

  #get single subclass value  
  warr1 = matrix(c(82), nrow=1, ncol=1) 
  warr2 = matrix(c(3), nrow=1, ncol=1)
  warr3 = matrix(c(2), nrow=1, ncol=1)
  outMatrix = matrix( -1, nrow=1, 1)
  
  for (i in 1:nrow(lims))
  {
    #Texture
    inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i])
    #Surface Condition
    #inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i] & warr3 >= lims$QualVal3_min[i] & warr3 <= lims$QualVal3_max[i])
    
    #Moisture avail 1.0
    #inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
    
    outMatrix[inds] <- as.integer(lims[landuse][i,])
  }
  outMatrix[rowid,]
  fOut <- cbind(cellids, outMatrix)
  Mode(fOut[rowid,])
  
  
  combDF <- data.frame(warr1[rowid,], warr2[rowid,], outMatrix[rowid,])
  table(outMatrix[rowid,])
  
  
  atvals1 <- readRDS('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Attributes/AWC100/Fitzroy/Chunks/v_150.rds')
  rowid <- which(atvals1[,1] == chunckCellInd)
  atval1<- atvals1[rowid,]
  
  median(warr1[rowid,])
  
  df <- cbind(warr1[rowid,], outMatrix[rowid,])
  
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
    print(tabulate(match(x, ux)))
  }
  
  
  
  #####
  ########### run a single limitation subclass file calculation 
  
  
  lim <- 'Climate-heat_dry_season_and_perennial'
  useList <- c('Gr-sorgh!dry!spr')
  att <- 'HotWet'
  
  suitfile <- 'NAWRA-Suitability_Subclasses_single_sheet_29-03-17.xlsx'
  suitfile <- paste0(catchmentName, '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
  sheet <- 'SUBCLASSES-2-M-linked'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  limRastersList <- getLimitationsPaths(rootDir = rootDir, mapFile = 'LimitationsFileMappings.xlsx')
  limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]
  k=chunk
  
 
  
  subs <- calcSubClasses(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, limInfo)
  rowid <- which(subs[,1] == chunckCellInd)
  valsl1 <- subs[rowid,]
  
  attvals <- readRDS(paste0(rootDir, '/attributes/' , att, '/', catchmentName, '/Chunks/r_', chunk, '.rds'))
  rowid <- which(as.numeric(rds[,1]) == chunckCellInd)
  
  
  valsDF <- data.frame(attvals, subs)
  head(subs)
  
  ######
  
  
  
  
  ##########    single landuse suitability    ################################
  
  landuse <- c('Maize!wet!spr')
  l <- getAvailableLimitations(catchmentName, landuse, suitFramework)
  currentLims <- l[l$avail & l$correctCnt,]
  
 df<- data.frame(id=seq(1,500))
  
  for(i in 1:nrow(currentLims)){
    
    rdsPath <- paste0(rootDir, '/Limitations/', catchmentName, '/', currentLims[i,1], '/', landuse, '/Chunks/subclasses_', chunk, '.rds')
    rds <- readRDS(rdsPath)
    #print(rds[chunckCellInd,])
    #subs <- rds[chunckCellInd,]
    
    subs <- rds[which(as.numeric(rds[,1]) == chunckCellInd),]
    
    #write.csv(subs, paste0(rootDir, '/temp/abcde.csv'))
    
    # which(as.numeric(rds[,1]) == chunckCellInd)
     c <- as.vector(subs)
     d <- c[-1]
    # limName <- currentLims[i,1]
    df <- cbind(df,d)
  }
 
 write.csv(df, paste0(rootDir, '/temp/suits.csv'))
  
  colnames(df) <- c('ID', as.character (currentLims$requiredLims))
    
  getModalValue(df$Irrigation_efficiency)
  
  suitvals <- apply(df[,-1],1,max)
  table(suitvals)
    
    table(df$Physical_restrictions)
    
    
    dfall <- cbind(df, suitvals)
    write.csv(dfall, paste0(rootDir, '/temp/allsuits.csv'))
  
  
  
  
  
    landuse <- c('Maize!wet!spr')
    rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
    catchmentName <- 'Darwin'
    outDir <- paste0( rootDir, '/temp' )
  
  suitFile <- paste0(catchmentName, '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
  sheet <- 'SUBCLASSES-2-M-linked'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  AllLims <- readSuitLimitations(rootDir = rootDir,classFile = 'LimitationRanges.xlsx')
  
  outDFLim <- data.frame(Landuse = character(), Limitation = character(), Subclass_Value = numeric(), Confusion_Index = numeric())
  
      l <- getAvailableLimitations(catchmentName, landuse, suitFramework)
      currentLims <- l[l$avail & l$correctCnt,]
       
      calculateOverallSuitabilitesModal(rootDir=rootDir, landuse=landuse, catchmentName=catchmentName, currentLims=currentLims, outDir=outDir)
      
      
      
      
      calculateOverallSuitabilitesModal(rootDir, landuse, catchmentName, currentLims, outDir)

      
      
      
     b<-  readRDS('V:/Projects/NAWRA/Production/temp/suit_3.rds')
      
      
      
      
      
         
  
  