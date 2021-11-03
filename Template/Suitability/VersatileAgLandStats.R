
#################################
###  Author : Ross Searle         
###  Date : Fri Sep 24 14:43:29 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Calculates the Ag Land versatile crop land maps
#################################


library(raster)
library(rgdal)
library(stringr)
library(readxl)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


rootDir = 'C:/Projects/Roper'
scriptRootDir <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/RoperLandSuit'
source(paste0(scriptRootDir, '/Suitability/SuitUtils_Roper.R'))



lessThan3 <- function(x) { 
  length(which(x <= 3 ))
}


suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
allsuits <- sort(getSuitList(suitFramework))
#write.csv(allsuits, 'c:/temp/suits.csv')
vsuits <- allsuits[c(12,15,22,39,37,26,32,11,49,9,18,52,24,1)]

srcDir <- paste0(rootDir, '/Suitability/Maps')
destDir <- paste0(rootDir, '/Versatile')
if(!dir.exists(destDir)){dir.create(destDir)}

files <- paste0(srcDir, '/', vsuits, '_m.tif' )
maskR <- raster('C:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')



theStack = stack(files)
# for (i in 1:length(files))
# {
#   path <- files[i]
#   print(path)
#   theStack<- addLayer(theStack,raster(path))
# }
names(theStack)

lt3 <-calc(theStack, fun=lessThan3)
m1 <- mask(lt3, maskR )
plot(m1)
writeRaster(m1,  paste0(rootDir, '/Versatile/versatile_14.tif'), overwrite =T)

############# Versatile by irrigation Type  #############
filesList <- list.files(srcDir, pattern = '_m.tif$', recursive = F, full.names = T)


#### Spray Irrigation   ######
theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!spr'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
uses <- names(theStackFilt)
length(uses)

SpraySuit <- calc(theStackFilt, fun=lessThan3)
m1 <- mask(SpraySuit, maskR)
plot(m1)
writeRaster(m1,  paste0(rootDir, '/Versatile/vSpray.tif'), overwrite =T)


#### Furrow/Flood Irrigation   ######
theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!fur'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
  
  if(str_detect(filesList[i], '!flood'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}

uses <- names(theStackFilt)
length(uses)

FurSuit <- calc(theStackFilt,fun=lessThan3)
m1 <- mask(FurSuit, maskR )
writeRaster(m1,  paste0(rootDir, '/Versatile/vFurrow.tif'), overwrite =T)


#### Trickle/Mini Spray Irrigation   ######
theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!tri'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
  
  
  if(str_detect(filesList[i], '!mini-spr'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}

uses <- names(theStackFilt)
length(uses)

TrickleSuit <- calc(theStackFilt, fun=lessThan3)
m1 <- mask(TrickleSuit, maskR)
writeRaster(m1,  paste0(rootDir, '/Versatile/vTrickle.tif'), overwrite =T)



theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!rainfed'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}

uses <- names(theStackFilt)
length(uses)

RainSuit <- calc(theStackFilt, fun=lessThan3)
m1 <- mask(RainSuit, maskR)
writeRaster(m1,  paste0(rootDir, '/Versatile/vRainfed.tif'), overwrite =T)




###############################################################################################

#End of Versatility processing

################################################################################################






plotDir <- paste0(rootDir, '/Versatile/', )
filesList <- list.files(plotDir, pattern = '.tif$', recursive = F, full.names = T)
theStackPlot = stack()
for (i in 1:length(filesList))
{
  theStackPlot<- addLayer(theStackPlot,raster(filesList[i]))
}

plot(theStackPlot)


# plot a bit more manually

catchment <- 'Darwin'
catchment <- 'Fitzroy'
catchment <- 'Mitchell'
dbdy <- readOGR(dsn=paste0(rootDir, '/Boundaries'), layer='Darwin_region_AWRC94')
dbdy <- readOGR(dsn=paste0(rootDir, '/Boundaries'), layer='Fitzroy_LSbdy94')
dbdy <- readOGR(dsn=paste0(rootDir, '/Boundaries'), layer='Mitchell_AWRC94')

mask <- raster(paste0(rootDir, '/Masks/', 'template_', catchment, '_geo.tif'))

r14 <- raster(paste0(rootDir, '/Versatile/', catchment, '/', catchment, '_v14.tif'))
r14m <- mask(r14, mask)
plot(r14m, main=paste0("Versatile Ag Land for ", catchment))
lines(dbdy)

par(mfrow=c(2,2))

r14 <- raster(paste0(rootDir, '/Versatile/', catchment, '/', catchment, '_vFurrow.tif'))
r14m <- mask(r14, mask)
plot(r14m, main=paste0("Versatile Ag Land for Furrow Irrig - ", catchment))
lines(dbdy)

r14 <- raster(paste0(rootDir, '/Versatile/', catchment, '/', catchment, '_vRainfed.tif'))
r14m <- mask(r14, mask)
plot(r14m, main=paste0("Versatile Ag Land for Rainfed - ", catchment))
lines(dbdy)

r14 <- raster(paste0(rootDir, '/Versatile/', catchment, '/', catchment, '_vSpray.tif'))
r14m <- mask(r14, mask)
plot(r14m, main=paste0("Versatile Ag Land for Spray Irrig - ", catchment))
lines(dbdy)

r14 <- raster(paste0(rootDir, '/Versatile/', catchment, '/', catchment, '_vTrickle.tif'))
r14m <- mask(r14, mask)
plot(r14m, main=paste0("Versatile Ag Land for Trickle Irrig - ", catchment))
lines(dbdy)

par(mfrow=c(1,1))


































filesList <- list.files(destDir, pattern = 'suitability_Cat.tif$', recursive = F, full.names = T)

theStack = stack()
for (i in 1:length(filesList))
#for (i in 1:5)
{
  theStack<- addLayer(theStack,raster(filesList[i]))
}

avgSuitR2 <- calc(theStack, fun=mean)

sumSuit <- sum(theStack)
avgSuitR <- sumSuit / nlayers(theStack)
plot(avgSuitR)

vals <- c(3,1,5,4,1,2, 3)

lt3 <-calc(theStack, fun=lessThan3)
plot(lt3)
lines(dbdy)

lt4 <-calc(theStack, fun=lessThan4)
plot(lt4)
lines(dbdy)

lessThan3 <- function(x) { 
  length(which(x <= 3 ))
}

lessThan4 <- function(x) { 
  length(which(x <= 4 ))
}



# Avg suit per irrigation type


theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!spr'))
  theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
SpraySuit <- calc(theStackFilt, fun=mean)
plot(SpraySuit)
lines(dbdy)

theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!fur'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
FurSuit <- calc(theStackFilt, fun=mean)
plot(FurSuit)
lines(dbdy)

theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!tri'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
TrickleSuit <- calc(theStackFilt, fun=mean)
plot(TrickleSuit)
lines(dbdy)


theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!rainfed'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
RainSuit <- calc(theStackFilt, fun=mean)
plot(RainSuit)
lines(dbdy)










#Average Suit per season

theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!per'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
PerSuit <- calc(theStackFilt, fun=mean)
plot(PerSuit)
lines(dbdy)

theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!wet'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
WetSuit <- calc(theStackFilt, fun=mean)
plot(WetSuit)
lines(dbdy)

theStackFilt = stack()
for (i in 1:length(filesList))
{
  if(str_detect(filesList[i], '!dry'))
    theStackFilt<- addLayer(theStackFilt,raster(filesList[i]))
}
DrySuit <- calc(theStackFilt, fun=mean)
plot(DrySuit)
lines(dbdy)



 # Versatile lands - selected crops
selCropSpray <- c(3,5,8,13,16,23,41,46,50,75,83,99,121, 125, 26)

substack <- stack(theStack@layers[selCropSpray])
VersitileSuit <- calc(substack, fun=mean)
plot(VersitileSuit)
lines(dbdy)

VersitileSuitLT3 <-calc(substack, fun=lessThan3)
plot(VersitileSuitLT3)
lines(dbdy)

names(substack)

# Uncertainty summaries

filesList <- list.files(destDir, pattern = 'suitability_Cat_CI.tif$', recursive = F, full.names = T)
theStackUncert = stack()
for (i in 1:length(filesList))
{
  theStackUncert<- addLayer(theStackUncert,raster(filesList[i]))
}


mUncert <-calc(theStackUncert, fun=mean)
stdUncert <-calc(theStackUncert, fun=sd)







