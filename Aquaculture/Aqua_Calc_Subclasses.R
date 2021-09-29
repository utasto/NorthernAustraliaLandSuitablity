
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 10:54:44 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Calculates the Aquaculture land suitabilites
#################################


library(raster)
library(rgdal)
library(stringr)
library(readxl)
library(leaflet)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}


rootDir <- 'C:/Projects/Roper/Aquaculture'

suitFrame <- as.data.frame( read_excel(paste0(rootDir, '/Aquaculture_Limitations.xlsx'), col_names = T, sheet = 'Aquaculture'))
uses <- c('Marine_EARTHEN',	'Marine_LINED',	'Fresh_EARTHEN','Fresh_LINED' )	

limcode <- c('slope','DistToWater','Elevation','pH','Clay','ASS','Sodicity','Rockiness','Gilgai', 'Permeability', 'SoilDepth')
rasterName <- c('slopePercHorneGeo','distToWaterGeoMasked','Relief_demcrop','ph2w','cl1','Relief_demcrop','SGG2b2','Rock2w','Micro1w','Perm3', 'db1_2')
rdf <- data.frame(limcode, rasterName)
rdf

########   Run the raster suitability calculations   #########

catchments <- c( 'Roper' )

for(z in 1:length(catchments)){
  
  catchment <- catchments[z]
  
  rasterDir <- paste0(rootDir, '/Attributes')
  LimRasterDir <- paste0(rootDir, '/LimitationRasters')
  if(!dir.exists(LimRasterDir)){dir.create(LimRasterDir)}
  SuitRasterDir <- paste0(rootDir, '/Suitability')
  if(!dir.exists(SuitRasterDir)){dir.create(SuitRasterDir)}

    for (k in 2:length(uses)){
      
      use <- uses[k]
      print(use)
          theStack <- stack()
          for(i in 1:nrow(rdf)){
            lim <- rdf[i,]$limcode
            
            all <- suitFrame[suitFrame$LIM_CODE == lim,]
            LimRules <- data.frame(all$LIM_CODE, all$Qual_Lower, all$Qual_Upper, all[use])
            
            if( !all(is.na( LimRules[use]))){
              
                print(paste0('Processing : ', catchment, ' - ', use, ' - ', lim))
                rasterFile <- rdf[i,]$rasterName
                print(paste0('Raster file = ', rasterFile))
                names(LimRules) <- c('LIM_CODE', 'Qual_Lower','Qual_Upper', 'sub')
                
                rlim <- raster(paste0(rasterDir, '/', rasterFile, '.tif' ))
                outR <- raster(rlim)
               
                for(j in 1:nrow(LimRules)){
                  print(paste0(lim , ' > ', LimRules$Qual_Lower[j], ' and <= ', LimRules$Qual_Upper[j]))
                  inds <- which(rlim[] > LimRules$Qual_Lower[j] & rlim[] <= LimRules$Qual_Upper[j])
                  outR[inds] <- as.integer(LimRules$sub[j] )
                }
              
                subclassDir<- paste0( LimRasterDir, '/', use)
                 if(!dir.exists(subclassDir)){
                   dir.create(subclassDir, recursive = T)
                 }
                 writeRaster(outR, paste0(subclassDir, '/subclass_', use, '_',lim, '.tif' ))
                 names(outR) <- lim
                 theStack<- addLayer(theStack,outR)
                }
          }

          print((theStack))
          suitR <- calc(theStack, fun=max)
          
          if(!dir.exists(SuitRasterDir)){
            dir.create(SuitRasterDir, recursive = T)
          }
          writeRaster(suitR, paste0(SuitRasterDir, '/suit_', use, '.tif'))
    }
}


###### Mask the raster to the catchment  ##########################

m <- raster('C:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')
fs <- list.files(SuitRasterDir, full.names = T, pattern = '.tif$')
for(i in 1: length(fs)){
  r <- raster(fs[i])
  print(names(r))
  mask(r, m, filename=paste0(SuitRasterDir, '/', names(r), '_m.tif'))
}


########  Make some images  #############################################

      Scols <- c("darkgreen", "green", "lightgreen", "orange", "brown")
      suitbrks <- c( 1, 2, 3, 4, 5)
      Sfactpal <- colorFactor(Scols, suitbrks,na.color = "transparent")
      suitLabs <- c("1", "2", "3", "4", "5")
      n <- c(0,1,2,3,4,5)
      

    fs <- list.files(SuitRasterDir, full.names = T, pattern = '_m.tif$')
    pStack = stack()
    print(paste0('Generating images for ', catchment))
    for(i in 1: length(fs)){
      r<- raster(fs[i])
      rn <- str_replace(basename(fs[i]), '.tif', '')
      names(r) <- rn
     # pStack<- addLayer(pStack,r)
      
      od <- paste0(rootDir, '/Maps')
      if(!dir.exists(od)){dir.create(od)}
      png(filename = paste0(od, '/', catchment, '_', rn, '.png'),   width = 20, height = 20, units = "cm", res=300)
      print(paste0('Plotting ', rn))
      plot(r, main=paste0(rn), col=desat(Scols,1) , breaks=n)
      
      dev.off()
    }









