library(raster) 
library(rgdal)
library(data.table)
library(reshape2)


source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/scripts/suitability/suitAnalysisUtils.R')

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

catchmentName <- 'Mitchell'
#catchmentName <- 'Fitzroy'

catBdy <- readOGR(dsn='//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Boundaries', layer=paste0(catchmentName, '_AWRC94'))
validSitesPts <- readOGR(dsn=paste0(rootDir, '/ValidationTrips/', catchmentName), layer='NAWRAQ_sites')
plot(catBdy)
points(validSitesPts)

#AllUses <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/PriorityLanduses.xlsx'), sheet='PriorityUses', col_names = T))

suitFile <- paste0(catchmentName, '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
usesl <- readSuitLanduses(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
uses <- data.frame(usesl)

#m <- as.data.frame(matrix(0, ncol = nrow(uses), nrow = 1))
m <- as.data.frame(matrix(0, ncol = nrow(uses), nrow = 1))
colnames(m) <- uses$usesl
m2 <- m[-1,]
n <- validSitesPts@data[0,]
outDF <- cbind(n, m2)
outDFU <- cbind(n, m2)

attributeNamesRaw <- readAttributeFileMapppings()
attributeNames <- unique(data.frame(Catchment=attributeNamesRaw$Catchment, FrameworkName=attributeNamesRaw$FrameworkName, ChunkName=attributeNamesRaw$ChunkName))
atts <- attributeNames[attributeNames$Catchment == catchmentName & attributeNames$ChunkName != '???',   ]
mA <- as.data.frame(matrix(0, ncol = nrow(atts), nrow = 1))
colnames(mA) <- atts$FrameworkName
outDFA <- cbind(n, mA[-1,])


for(i in 1:length(validSitesPts)){
  #for(i in 1:2){
  
  print(i)
  
  allsuitData <- getSuitValues(rootDir=rootDir, lon=validSitesPts@coords[i,1], lat=validSitesPts@coords[i,2], catchmentName=catchmentName, landuses=uses, doAllLims=F )
  allattData <- getAttributeValues(rootDir=rootDir, lon=validSitesPts@coords[i,1], lat=validSitesPts@coords[i,2], catchmentName=catchmentName, attributeNames = atts)
  
  #allsuitData <- getSuitValues(rootDir=rootDir, lon=validSitesPts@coords[i,1], lat=validSitesPts@coords[i,2], catchmentName=catchmentName, landuses=AllUses[catchmentName], doAllLims=F )
  dfLoc <- allsuitData[[1]]
  dflims <- allsuitData[[2]]
  dfAtts <- allattData@Attributes
  dfsuit <- allsuitData[[4]]
  
  dfl <- data.frame(dfsuit$Suitability)
  rownames(dfl) <- dfsuit$LandUse
  dflt <- t(dfl)
  outRow <- cbind(validSitesPts@data[i,], dflt)
  outDF <- rbind(outDF, outRow)
  
  dfU <- data.frame(dfsuit$Confusion_Index)
  rownames(dfU) <- dfsuit$LandUse
  dfltU <- t(dfU)
  outRowU <- cbind(validSitesPts@data[i,], dfltU)
  outDFU <- rbind(outDFU, outRowU)
  
  
  dfA <- data.frame(dfAtts$Att_Value)
  rownames(dfA) <- dfAtts$Attribute
  dfltA <- t(dfA)
  outRowA <- cbind(validSitesPts@data[i,], dfltA)
  outDFA <- rbind(outDFA, outRowA)
  print(outDFA)
}




outDir <- 'V:/projects/NAWRA/Production/ValidationTrips/Mitchell'
pName <- 'Ben'
coordinates(outDF) <- ~ o_longitud + o_latitude
proj4string(outDF) <- CRS("+proj=longlat +datum=WGS84")

writeOGR(outDF, outDir, pName, driver="ESRI Shapefile", overwrite_layer=TRUE)
cat(showWKT(proj4string(outDF)),file=paste0(outDir, '/', pName, '.prj') )

b<-as.data.frame(outDF)
write.csv(b, paste0(outDir, '/Mitchell_All_Suits.csv' ))

u<-as.data.frame(outDFU)
write.csv(u, paste0(outDir, '/Mitchell_SuitUncert.csv' ))

a<-as.data.frame(outDFA)
write.csv(a, paste0(outDir, '/Mitchell_Attributes.csv' ))


