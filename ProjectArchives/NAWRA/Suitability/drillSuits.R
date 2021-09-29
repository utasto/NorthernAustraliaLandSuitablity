
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/scripts/suitability/suitAnalysisUtils.R')

lat <- -13.096
lon <- 131.422

lon <- 131.9


rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'


#suitability Subclasses
catchmentName <- 'Darwin'
landuse <- 'Cane!wet-long!rainfed'
dataType <- 'Limitations'
dataSubType <- 'subclasses' #'subclass_Suit' 'suit'
limitation <- 'Climate-annual_rainfall'


#suitability Subclass_Suit
catchmentName <- 'Darwin'
landuse <- 'Cane!wet!rainfed'
dataType <- 'Limitations'
dataSubType <- 'subclass_Suit' 
limitation <- 'Climate-annual_rainfall'

#suitability Subclass_Suit - ALL
catchmentName <- 'Darwin'
landuse <- 'Cane!wet-long!spr'
dataType <- 'Limitations'
dataSubType <- 'subclass_Suit' 
limitation <- 'All'


# overall suitability
catchmentName <- 'Darwin'
landuse <- 'Cane!wet!rainfed'
dataType <- 'Suitability'
dataSubType <- 'suit' 

# overall suitability - ALL
catchmentName <- 'Darwin'
landuse <- 'All'
dataType <- 'Suitability'
dataSubType <- 'suit' 


xxx()

xxx <- function(){
lims <- getSuitValues(rootDir=rootDir, lon=lon, lat=lat, catchmentName=catchmentName, landuse=landuse, limitation='All', dataType='Limitations', dataSubType='subclass_Suit')
suit <- getSuitValues(rootDir=rootDir, lon=lon, lat=lat, catchmentName=catchmentName, landuse=landuse, limitation=limitation, dataType='Suitability', dataSubType=dataSubType)


print(paste0('Catchment =  ', suit@location@Catchment , ' Lat : ', suit@location@Lat, ' Lon : ', suit@location@Lon))
print( paste0('Landuse = ', suit@location@Landuse, '  Suitability = ', suit@Values$Suitability, ' CI = ', suit@Values$Confusion_Index ))
#m <- paste0(m, as.character(lims@Values))
print(lims@Values)
#cat(as.character(lims@Values))

}







