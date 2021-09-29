
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')



d<-readRDS('V:/Projects/NAWRA/Production/Limitations/Mitchell/Moisture_availability_1.0/Cotton!wet!spr/Chunks/subclasses_80.rds')
d

d2<-readRDS('V:/Projects/NAWRA/Production/Limitations/Mitchell/Moisture_availability_1.0/Cotton!wet!spr/Chunks/subclass_Suit_80.rds')
d2
# bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
# limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]
# 


catchmentName <- 'Mitchell'
lim <- 'Moisture_availability_1.0'
landuse <- 'Cotton!dry!spr'
useList <- c(landuse)

landuses <- 'Cane!wet!fur;Cane!wet!spr;Cane!wet!rainfed;Cotton!dry!fur;Cotton!dry!spr;Cotton!wet!rainfed;Cotton!wet!fur;Cotton!wet!spr;Poppies!dry!fur;Poppies!dry!spr;Rice-flood!wet!flood;Rice!wet!fur;Rice-upland!wet!spr;Rice!wet!rainfed;Rice!dry!flood;Rice!dry!fur;Rice-upland!dry!spr;Maize!wet!fur;Maize!wet!spr;Maize!wet!rainfed;Maize!dry!fur;Maize!dry!spr;Gr-sorgh!wet!fur;Gr-sorgh!wet!spr;Gr-sorgh!wet!rainfed;Gr-sorgh!dry!fur;Gr-sorgh!dry!spr;Millet!wet!fur;Millet!wet!spr;Millet!wet!rainfed;Millet!dry!fur;Millet!dry!spr;Chickpea!dry!fur;Chickpea!dry!spr;Chickpea!wet-dr!rainfed;Soybean!wet-dr!rainfed;Soybean!dry!fur;Soybean!dry!spr;Mungbean!wet-dr!rainfed;Mungbean!dry!fur;Mungbean!dry!spr;Navybean!wet-dr!rainfed;Navybean!dry!fur;Navybean!dry!spr;Lentil!dry!fur;Lentil!dry!spr;Lentil!wet-dr!rainfed;Chia!dry!fur;Chia!dry!spr;Chia!wet-dr!rainfed;Quinoa!dry!fur;Quinoa!dry!spr;Fr-sorgh!wet!fur;Fr-sorgh!wet!spr;Fr-sorgh!wet!rainfed;Fr-sorgh!dry!fur;Fr-sorgh!dry!spr;Rhodes!wet!fur;Rhodes!wet!spr;Lablab!wet!fur;Lablab!wet!spr;lablab!wet!rainfed;Lablab!dry!fur;Lablab!dry!spr;Maize-sil!wet!fur;Maize-sil!wet!spr;Maize-sil!wet!rainfed;Maize-sil!dry!fur;Maize-sil!dry!spr;Sunflower!wet!fur;Sunflower!wet!spr;Sunflower!dry!fur;Sunflower!dry!spr;Sesame!wet!fur;Sesame!wet!spr;Sesame!wet!rainfed;Sesame!dry!fur;Sesame!dry!spr;Capsicum!dry!fur;Capsicum!dry!spr;Capsicum!dry!tri;Cucumber!wet!spr;Cucumber!wet!tri;Cucumber!wet!rainfed;Cucumber!dry!spr;Cucumber!dry!tri;Sweetcorn!wet!fur;Sweetcorn!wet!spr;Sweetcorn!dry!fur;Sweetcorn!dry!spr;Tomato!dry!tri;Asian-veg!dry!tri;Asian-snake!wet!tri;Asparagus!dry!spr;Asparagus!dry!tri;Peanut!wet!fur;Peanut!wet!spr;Peanut!wet!rainfed;Peanut!dry!fur;Peanut!dry!spr;Sweet-pot!dry!fur;Sweet-pot!dry!spr;Cassava!wet!fur;Cassava!wet!spr;Cassava!wet!rainfed;Indian-Sand!per!fur;Indian-S!per!tri;African-M!per!fur;African-M!per!tri;Teak!per!fur;Teak!per!tri;Cashew!per!spr;Cashew!per!tri;Mango!per!spr;Mango!per!tri;Banana!per!spr;Banana!per!tri;Citrus!per!tri;Coffee!per!tri;coffee!per!tri;Avocado!per!tri;Lychee!per!tri;Papaya!per!spr;Papaya!per!tri;Macadamia!per!tri;Almond!per!tri'

useList <- unlist(c(str_split(landuses, ';')))
bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]

suitFile <- paste0(catchmentName, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
limRastersList <- getLimitationsPaths(rootDir = rootDir, mapFile = 'LimitationsFileMappings.xlsx')


for(k in 1:bs$n){
  k=1
  processSubClassesParallel(rootDir, catchmentName, lim, useList, suitFramework, limRastersList, bs, limInfo)
  
}

suitFile <- paste0(cats[i], '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitFile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
usesstring <-  paste(uses, collapse=";") 




 r <- readRDS('v:/Projects/NAWRA/Production/Limitations/Darwin/Wetness/African-M!per!fur/Chunks/subclasses_7.rds')
 r
  
 r2 <- r[,-1]
 summary(as.vector(r))
  table(r)
  
  
  numcpus=10
  catchmentName <- 'Fitzroy'
  lim <- 'Physical_restrictions'
  landuses <- 'Cane!wet!fur'
  
  
  bob <- readRDS('Q:/Projects/NAWRA/Production/Suitability/Darwin/Cane!wet-long!rainfed/Chunks/suit_15.rds')
  

  
  
  
  rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
  
  lim <- 'Erosion'
  lim<- "Physical_restrictions"
  lim<- "Climate-heat_wet_season_and_perennial"
  lim <- 'Wetness'
  lim <- 'Moisture_availability_RAINFED_CROPPING_10'
  lim <- 'Moisture_availability_RAINFED_CROPPING_15'
  
  landuses <- c('Maize!wet!fur')
  landuses <- c('Cotton!wet!rainfed')
  useList <- unlist(c(str_split(landuses, ';')))
  
  
  suitfile <- 'NAWRA-Suitability_Subclasses_single_sheet_29-03-17.xlsx'
  suitfile <- paste0(catchmentName, '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
  sheet <- 'SUBCLASSES-2-M-linked'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  limRastersList <- getLimitationsPaths(rootDir = rootDir, mapFile = 'LimitationsFileMappings.xlsx')
    bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
  limInfo <- suitFramework[ suitFramework$LIM_Name==lim,]
  
  r <- readRDS('')
  rs <- readRDS('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Limitations/darwin/Moisture_availability_RAINFED_CROPPING_10/Cotton!wet!rainfed/Chunks/subclasses_5.rds')
  
  awc100 <- readRDS('V:/Projects/NAWRA/Production/Attributes/AWC100/Darwin/Chunks/r_122.rds')
  rain<- readRDS('V:/Projects/NAWRA/Production/Attributes/Rain/Darwin/Chunks/r_122.rds')
 
  a <- awc100[2,5]
  r <- rain[2,5]
  
 r <- readRDS( '\\OSM-09-CDC.it.csiro.au\OSM_CBR_LW_SLGA_work\Projects\NAWRA\Production\Limitations\Darwin\Moisture_availability_RAINFED_CROPPING_10\Quinoa!wet-dr!rainfed\Chunks\subclasses_3.rds')
 
 