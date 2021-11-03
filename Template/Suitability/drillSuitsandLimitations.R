
#################################
###  Author : Ross Searle         
###  Date : Mon Sep 27 08:42:30 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Drill the limitation rasters and calculate the suitability at a point.
###  Notes : run on TernSoil2 VM
#################################

library(raster)
library(stringr)
library(sf)

rasterOptions(timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) 

rootDir <- 'N:/3_Land_suitability/0_Working/Uta/Roper'
scriptsPath <- 'Y:/Ross/Roper/Scripts/RoperLandSuit'

#source(paste0(scriptsPath, '/Mapping/RandomForestUtils_Roper.R'))
source(paste0(scriptsPath, '/Suitability/SuitUtils_Roper.R'))


# ids <- c('A', 'B')
# lats <- c(-15, -14)
# lons <- c(132, 133)
# locs <- data.frame(ids, lons, lats, stringsAsFactors = F)

locs <- st_read(paste0(rootDir, '/Suitability/Checking/pulseCheck.shp'))


suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(rootDir, '/Suitability/LimitationsFileMappingsRoper2.xlsx')
suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

attOther <- c("Rain","tmax_DryGreaterThan40_Avg","tmax_WetGreaterThan40_Avg","tmin_LessThan2Count_Avg",  
              "tmin_MonthsLessThan15_Avg","slopedeg_1s_Roper","Relief_demcrop" )
attPaths <- unique(limRastersList$ChunkName)

attStk <- stack()
for (i in 1:length(attPaths)) {
  print(i)
  att <- attPaths[i]
  if(att %in% attOther){
    attPath <- paste0(rootDir, '/ClimateAndReliefMasked/', att,'.tif' )
  }else{
    attPath <- paste0(rootDir, '/Maps/', att, '/',att,'.tif' )
  }
  r <- raster(attPath)
  attStk <- addLayer(attStk, r)
}


#drillDF <- extract(attStk, locs[,c(2,3)])
drillDF <- extract(attStk, locs)
locs2 <- data.frame(id=locs$id, st_coordinates(locs))
limDF <- cbind(locs2,drillDF)
names(limDF) <- str_replace_all(names(limDF), '[.]', '_')
names(limDF)[19] <- 'Micro1w'


limsToDo <- unique(suitFramework$LIM_Name)
useList <- getSuitList(suitFramework)


landuses <- c("Lablab!dry!fur","Lablab!dry!spr","lablab!wet!rainfed")
landuses <- c("Cane!wet-long!spr","Cane!wet-long!rainfed" )
landuses <- c("Cotton-grains!wet!rainfed","Sunflower-Sesame!wet!rainfed","Rice_upland!wet!rainfed","Chia-Quinoa-Poppies!wet-dr!rainfed",
              "Pulse_crops!wet-dr!rainfed", "Forage_Sorghum-Maize!wet!rainfed","lablab!wet!rainfed"  )

landuse <- "Lablab!dry!fur"                    
landuse <- "Lablab!dry!spr"                    
landuse <- "lablab!wet!rainfed" 
landuse <- "Forage_Sorghum-Maize!wet!rainfed"

for(j in 1:length(landuses)){
  
  landuse <- landuses[j]
  
  attsOut <- limDF
  
  ur <- raster(paste0(rootDir, '/Suitability/Maps/', landuse, '_m.tif'))
  drillUse <- extract(ur, locs)
  attsOut[landuse] <- drillUse
  
  
  for (i in 1:length(limsToDo)) {
    lim <- limsToDo[i]
    limInfo <- suitFramework[suitFramework$LIM_Name==lim,]
    subs<- calcSubClassesAtaPoint(lim=lim, uses=landuse, atts=limDF, suitFramework, limInfo)
    attsOut[lim] <- subs
    
  }
  
  odf <- as.data.frame(t(attsOut))
  odf
  write.csv(odf, paste0(rootDir, '/Suitability/Checking/subclasses_', landuse, '.csv'))
  
}
