
library(raster)
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')


coord <- setClass("coord", slots = c(Lat="numeric", Lon="numeric", Landuse="character", Catchment="character"))
SuitDrill <- setClass("SuitDrill", slots = c(SubClasses="data.frame", Attributes="data.frame", Suits="data.frame", location="coord"))



# lat <- -13.096
# lon <- 131.422
# 
# lat <- -12.042367
# lon <- 131.297355
# bob<-getAttributeValues(rootDir, lon, lat, catchmentName)

getAttributeValues <- function(rootDir=NULL, lon=NULL, lat=NULL, catchmentName=NULL, attributeNames=NULL ){
  
  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  templateR <- raster(paste0(rootDir, '/Masks/template_' , catchmentName,'_geo.tif'))
  cellFromXY(templateR, cbind(c(lon), c(lat)))
  col <- colFromX(templateR, lon)
  row <- rowFromY(templateR, lat)
  chunk <- ( (row-1) %/% bs$nrows[1]) + 1
  chunkline <- ((row-1) %% bs$nrows[1]) + 1
  chunckCellInd <- ((chunkline-1) * ncol(templateR)) + col
  

  
  c <- coord(Lat = lat, Lon = lon, Catchment = catchmentName)
  
  c=NULL
  outDFATTS=NULL
  
  c <- coord(Lat=lat, Lon=lon, Catchment=catchmentName)
  
  
  outDFATTS <- data.frame( Attribute = character(), Att_Value = numeric(), Uncert = numeric())
  
    atts <- attributeNames[attributeNames$Catchment == catchmentName & attributeNames$ChunkName != '???',   ]
    colnames(atts) <- c('Catchment', 'FrameworkName','ChunkName')
    for(j in 1:nrow(atts)){
      
      if(atts$FrameworkName[j] %in% c('PAWC15', 'PAWC10', 'PAWC06', 'pH', 'SoilDepth', 'DepthA', 'ESP')){
        
        rds <- readRDS(paste0(rootDir, '/attributes/' , atts$ChunkName[j], '/', catchmentName, '/Chunks/r_', chunk, '.rds'))
        rowid <- which(as.numeric(rds[,1]) == chunckCellInd)
        vals2 <- rds[rowid,-1]
        #m <- mean(na.omit(vals2))
        m<-median(na.omit(vals2))
        stdDev <- sd(vals2)
        ci <-(stdDev/m)*100
        outdf2 <- data.frame(Attribute=atts$FrameworkName[j], Att_Value = m, Uncert = ci)
        outDFATTS <- rbind(outDFATTS, outdf2)

      }
      else if(atts$FrameworkName[j] %in% c('DEM', 'Rainfall', 'HotDry', 'HotWet', 'Slope', 'Elevation', 'Frost', 'Cool')){
        rds <- readRDS(paste0(rootDir, '/attributes/' , atts$ChunkName[j], '/', catchmentName, '/Chunks/r_', chunk, '.rds'))
        rowid <- which(as.numeric(rds[,1]) == chunckCellInd)
        vals2 <- rds[rowid,-1]
        #m <- mean(na.omit(vals2))
        m<-median(na.omit(vals2))
        stdDev <- sd(vals2)
        ci <-(stdDev/m)*100
        outdf2 <- data.frame(Attribute=atts$FrameworkName[j], Att_Value = m, Uncert = ci)
        outDFATTS <- rbind(outDFATTS, outdf2)
      }
      else{
        
        
        rds <- readRDS(paste0(rootDir, '/attributes/' , atts$ChunkName[j], '/', catchmentName, '/Chunks/v_', chunk, '.rds'))
        rowid <- which(as.numeric(rds[,1]) == chunckCellInd)
        vals<- rds[rowid,]
        outdf2 <- data.frame( Attribute=atts$FrameworkName[j], Att_Value = vals$modalVal, Uncert = vals$ci)
        #outdf2 <- data.frame(Limitation=atts$Limitation[j], Attribute=atts$FrameworkName[j], Att_Value = m, Uncert = ci)
        outDFATTS <- rbind(outDFATTS, outdf2)
      }
    }
  
  sd <- SuitDrill(Attributes=outDFATTS, location=c )
}
#write.csv(outDFATTS, paste0(rootDir, '/temp/s1_atts.csv'))




getSuitValues <- function(rootDir=NULL, lon=NULL, lat=NULL, catchmentName=NULL, landuses=NULL, attributeNames = atts, doAllLims=F ){
  
  uses<-landuses
  
  
 # Site ID	  Day 1_705.csv 
  # lon <-	123.7317
  # lat<-	-18.4
  # catchmentName <- 'Fitzroy'
  # thisUse <- 'Banana!per!tri'
  # theLim <- 'Wetness'
  

  bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  templateR <- raster(paste0(rootDir, '/Masks/template_' , catchmentName,'_geo.tif'))
  cellFromXY(templateR, cbind(c(lon), c(lat)))
  col <- colFromX(templateR, lon)
  row <- rowFromY(templateR, lat)
  chunk <- ( (row-1) %/% bs$nrows[1]) + 1
  chunkline <- ((row-1) %% bs$nrows[1]) + 1
  chunckCellInd <- ((chunkline-1) * ncol(templateR)) + col
  
  c <- coord(Lat = lat, Lon = lon, Catchment = catchmentName)
  
  
  ######   Extract all the subclass limitation values for the listed landuses #################
  
      suitFile <- paste0(catchmentName, '_Suitability_Framework.xlsx')
      sheet <- 'SUBCLASSES-2-M-linked'
      suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
      AllLims <- readSuitLimitations(rootDir = rootDir,classFile = 'LimitationRanges.xlsx')
      
      outDFLim <- data.frame(Landuse = character(), Limitation = character(), Subclass_Value = numeric(), Confusion_Index = numeric())
      

      for(li in 1:nrow(uses)){

        thisUse <- as.character(uses[li,])


        currentLims <-NULL
        if(doAllLims){
          currentLims <- AllLims
        }else{
          l <- getAvailableLimitations(catchmentName, thisUse, suitFramework)
          currentLims <- l[l$avail,][,1]
        }


          for(i in 1: length(currentLims)){
              theLim = currentLims[i]
              rds <- readRDS(paste0(rootDir, '/Limitations/' , catchmentName, '/', theLim, '/', thisUse, '/Chunks/subclass_Suit_', chunk, '.rds'))
              rowid <- which(as.numeric(rds[,1]) == chunckCellInd)
              vals<- rds[rowid,]
              #vals<- rds[chunckCellInd,]
              outdf2 <- data.frame(Landuse=thisUse, Limitation=theLim, Subclass_Value=vals$modalVal, Confusion_Index=vals$ci)
              outDFLim <- rbind(outDFLim, outdf2)

            }
      }


      oat <-getAttributeValues(rootDir=rootDir, lon=lon, lat=lat, catchmentName=catchmentName, attributeNames = atts)
      outDFATTS <- oat@Attributes


############   Get the calculated suitabilities for listed land uses   #####################
      
      outDFsuit <- data.frame( LandUse = character(), Suitability = numeric(), Confusion_Index = numeric())
      
      
      for(i in 1:nrow(uses)){

        landuse = uses[i,]
        fn <- paste0(rootDir, '/Suitability/' , catchmentName, '/',  landuse, '/Chunks/suit_', chunk, '.rds')
        #print(fn)
        if(file.exists(fn)){
         
          rds <- readRDS(fn)
          rowid <- which(as.numeric(rds[,1]) == chunckCellInd)
          vals<- rds[rowid,]
          #vals<- rds[chunckCellInd,]
          
         # print(vals)
          
          outdf2 <- data.frame(LandUse = landuse, Suitability = vals$modalVal, Confusion_Index = vals$ci)
          outDFsuit <- rbind(outDFsuit, outdf2)
        }else{
          print(paste0('Missing  - ', landuse))
        }
      }

      return(list(c, outDFLim, outDFATTS,outDFsuit ))
     
}










