

TESTING <- F


if(TESTING == T){
  rootDirSweeps = 'D:/temp/sweeptest'
  rootDir = 'D:/Projects/NAWRA/Production'
}else{
  rootDirSweeps = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/LimitationSweeps'
  rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
}

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')

cats <- c('Darwin', 'Mitchell', 'Fitzroy')
cats <- c( 'Mitchell')
lims <- c('Climate-annual_rainfall')
lims <- c('Climate-heat_dry_season_and_perennial')
lims <- c('Climate-heat_wet_season_and_perennial')
#lims <- c('Climate-frost')
lims <- c('Climate-temp_variation')
#lims <- c('Wetness')
lims <- c('Moisture_availability_1.5')
lims <- c('Moisture_availability_1.0')
lims <- c('Moisture_availability_0.6')
lims <- c('Nutrient_balance')
#lims <- c('Soil_depth')
#lims <- c('Rockiness')
#lims <- c('Microrelief')
#lims <- c('Irrigation_efficiency')
lims <- c('Irrigation_efficiency-high_rate_methods')
lims <- c('Moisture_availability_RAINFED_CROPPING_15')
lims <- c('Moisture_availability_RAINFED_CROPPING_10')
lims <- c('Moisture_availability_RAINFED_CROPPING_06')
lims <- c('Acid_Sulfate_Soil_Potential')
#lims <- c('Physical_restrictions')
lims <- c('Erosion')
#lims <- c('Salinity_surface')

#lims <- c('Surface_Condition')
#lims <- c('Surface_Texture')
#lims <- c('Surface_Infiltration')


landuses <- "RerunsPoppy"


lims <- c('Moisture_availability_0.6', 'Moisture_availability_1.0','Moisture_availability_1.5','Surface_Condition', 'Surface_Texture','Surface_Infiltration')
lims <- c('Climate-annual_rainfall', 'Climate-heat_dry_season_and_perennial', 'Climate-heat_wet_season_and_perennial', 'Climate-frost','Climate-temp_variation',
          'Wetness','Moisture_availability_1.5', 'Moisture_availability_1.0', 'Moisture_availability_0.6', 
          'Nutrient_balance', 'Soil_depth', 'Rockiness', 'Microrelief', 'Irrigation_efficiency', 'Irrigation_efficiency-high_rate_methods',
          'Moisture_availability_RAINFED_CROPPING_15', 'Moisture_availability_RAINFED_CROPPING_10','Moisture_availability_RAINFED_CROPPING_06',
          'Acid_Sulfate_Soil_Potential', 'Erosion', 'Salinity_surface',
          'Surface_Condition', 'Surface_Texture','Surface_Infiltration')


###  ps and pa reruns
uses <- c('Cotton!dry!fur','Cotton!dry!spr','Cotton!wet!fur','Cotton!wet!spr','Cotton!wet!rainfed','Fr-sorgh!wet!fur','Fr-sorgh!wet!spr','Fr-sorgh!wet!rainfed','Fr-sorgh!dry!fur'
  ,'Fr-sorgh!dry!spr','Rhodes!wet-long!fur','Rhodes!wet-long!spr','Lablab!wet!fur','Lablab!wet!spr','lablab!wet!rainfed','Lablab!dry!fur'
  ,'Lablab!dry!spr','Maize-sil!wet!fur','Maize-sil!wet!spr','Maize-sil!wet!rainfed','Maize-sil!dry!fur','Maize-sil!dry!spr','Indian-Sand!per!fur'
  ,'Indian-S!per!mini-spr','African-M!per!fur','African-M!per!tri','Teak!per!fur','Teak!per!tri','Cashew!per!spr','Cashew!per!tri','Mango!per!spr'
  ,'Mango!per!tri','Banana!per!spr','Banana!per!tri','Citrus!per!mini-spr','Coffee!per!mini-spr','Avocado!per!mini-spr','Lychee!per!mini-spr'
  ,'Papaya!per!spr','Papaya!per!tri','Macadamia!per!mini-spr','Almond!per!mini-spr')

uses <- c('Poppies!dry!fur', 'Poppies!dry!spr')


lims <- c('Surface_Condition', 'Surface_Texture')

cats <- c('Darwin', 'Fitzroy', 'Mitchell')

#####   One node per landuse sweep generation

for (i in 1:length(cats)){
  
  suitfile <- paste0(cats[i], '_Suitability_Framework.xlsx')
  sheet <- 'SUBCLASSES-2-M-linked'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  #uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  usesstring <-  paste(uses, collapse=";") 
  
  for(j in 1:length(lims)){
    
    cnt<-1
    outDir <- paste0(rootDirSweeps, '/', cats[i], '/', lims[j])
    #outDir <- paste0(rootDirSweeps, '/', cats[i])
    dir.create(outDir, recursive = T)
    curLims <- suitFramework[suitFramework$LIM_Name==lims[j],]
    
    usesstring <- ''
    for(k in 1:length(uses)){
      landuse = uses[k]
      validInds <- which(!is.na(curLims[landuse]))
      
      if(length(validInds) > 0){
        #print(landuse)
        usesstring <- paste(usesstring,landuse, ';', sep='')
      }
    }
    
    usesstring <-  substr(usesstring, 1, nchar(usesstring)-1)
    useList <- unlist(c(str_split(usesstring, ';')))
    
    
    for(r in 1:length(useList)){
      use <- useList[r]
        #sname <- paste0('subclasses_', lims[j], '_', landuses)
        sname <- paste0('subclasses_', lims[j], '_', cnt)
        fname <- paste0(outDir, '/',sname, '.bat')
        
        unlink(fname)
        fileConn<-file(fname, open = 'a')
        writeLines('@echo off', fileConn)
        writeLines('echo %CCP_NUMCPUS%', fileConn)
        writeLines('set OMP_NUM_THREADS=%CCP_NUMCPUS%', fileConn)
        writeLines('set PATH=%PATH%;C:\\winapps\\R\\3.1.2\\bin\\x64', fileConn)
        writeLines(paste0('rscript \\\\OSM-09-CDC.it.csiro.au\\OSM_CBR_LW_SLGA_work\\projects\\NAWRA\\production\\scripts\\subclasses\\calculateSuitabilitySubclassesHPC2.R ', cats[i], ' ',lims[j], ' ', use), fileConn)
        close(fileConn)
        #print(paste0(cnt, ' : Sweep batch file written to ', fname))
        print(paste0(use, ' - ', r))
        cnt<-cnt+1
    }
    print(paste0('File count for ', cats[i], ' is ', cnt-1))
    
  }
}




















##### batch files for Grouped landuses
cnt<-1
for (i in 1:length(cats)){

  suitfile <- paste0(cats[i], '_Suitability_Framework.xlsx')
  sheet <- 'SUBCLASSES-2-M-linked'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  usesstring <-  paste(uses, collapse=";") 
  
  for(j in 1:length(lims)){
    
    #outDir <- paste0(rootDirSweeps, '/', cats[i], '/', lims[j])
    outDir <- paste0(rootDirSweeps, '/', cats[i])
    dir.create(outDir, recursive = T)
    curLims <- suitFramework[suitFramework$LIM_Name==lims[j],]
    
    usesstring <- ''
    for(k in 1:length(uses)){
      landuse = uses[k]
      validInds <- which(!is.na(curLims[landuse]))
      
      if(length(validInds) > 0){
        #print(landuse)
        usesstring <- paste(usesstring,landuse, ';', sep='')
      }
    }
    
    usesstring <-  substr(usesstring, 1, nchar(usesstring)-1)
    
    sname <- paste0('subclasses_', lims[j], '_', landuses)
    fname <- paste0(outDir, '/',sname, '.bat')
    
    unlink(fname)
    fileConn<-file(fname, open = 'a')
    writeLines('@echo off', fileConn)
    writeLines('echo %CCP_NUMCPUS%', fileConn)
    writeLines('set OMP_NUM_THREADS=%CCP_NUMCPUS%', fileConn)
    writeLines('set PATH=%PATH%;C:\\winapps\\R\\3.1.2\\bin\\x64', fileConn)
    writeLines(paste0('rscript \\\\OSM-09-CDC.it.csiro.au\\OSM_CBR_LW_SLGA_work\\projects\\NAWRA\\production\\scripts\\subclasses\\calculateSuitabilitySubclassesHPC2.R ', cats[i], ' ',lims[j], ' ', usesstring), fileConn)
    close(fileConn)
    print(paste0(cnt, ' : Sweep batch file written to ', fname))
    
    cnt<-cnt+1
    
  }
}
