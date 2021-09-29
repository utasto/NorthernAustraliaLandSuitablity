
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')

cats <- c('Darwin', 'Mitchell', 'Fitzroy')
expFileCnt <- c(396, 398 ,390)
fCnt <- data.frame(cats, expFileCnt)

lims <- c('Climate-annual_rainfall', 'Climate-heat_dry_season_and_perennial', 'Climate-heat_wet_season_and_perennial', 'Climate-frost','Climate-temp_variation',
          'Wetness','Moisture_availability_1.5', 'Moisture_availability_1.0', 'Moisture_availability_0.6', 
          'Nutrient_balance', 'Soil_depth', 'Rockiness', 'Microrelief', 'Irrigation_efficiency', 'Irrigation_efficiency-high_rate_methods',
          'Moisture_availability_RAINFED_CROPPING_15', 'Moisture_availability_RAINFED_CROPPING_10','Moisture_availability_RAINFED_CROPPING_06',
          'Acid_Sulfate_Soil_Potential', 'Erosion', 'Salinity_surface',
          'Surface_Condition', 'Surface_Texture','Surface_Infiltration')


rootDirSweeps = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Pearcy/LimitationSweeps'
rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
rootDirUnix ='/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production'
rootDirSweepsUnix = '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/Pearcy/LimitationSweeps'





cat <- 'Darwin'
cat <- 'Mitchell'
cat <- 'Fitzroy'


inDirRoot <- paste0(rootDir, '/Limitations/', cat)

outfname <- paste0(rootDir, '/Limitations/', cat, '_LimCheck.csv')
unlink(outfname)
fileConn<-file(outfname, open = 'wb')

suitfile <- paste0(cat, '_Suitability_Framework.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

tgtCnt <- as.numeric(fCnt[fCnt$cats == cat,][2])

for(i in 1:length(lims)){
 
  print(paste0('Processing ', lims[i]))
  
  if(!dir.exists(paste0(inDirRoot, '/', lims[i]))){
    writeLines(paste0('PROBLEM, ', 0, ',', 'Limitation not processed - ', lims[i]), fileConn)
  
    }else{
        
      OK <- T
        curLims <- suitFramework[suitFramework$LIM_Name==lims[i],]
        
        usesstring <- ''
        for(k in 1:length(uses)){
          landuse = uses[k]
          validInds <- which(!is.na(curLims[landuse]))
          
          if(length(validInds) > 0){
            usesstring <- paste(usesstring,landuse, ';', sep='')
          }
        }
        usesstring <-  substr(usesstring, 1, nchar(usesstring)-1)
        useList <- unlist(c(str_split(usesstring, ';')))
        
        
        for(j in 1:length(useList)){ 
        
        inDir <- paste0(inDirRoot, '/', lims[i],'/', useList[j], '/Chunks')
        
            if(!dir.exists(paste0(inDir))){
                  writeLines(paste0('PROBLEM, ', 0, ',', 'Landuse not processed - ', useList[j], ' for Limitation - ', lims[i]), fileConn)
              OK <- F
            }else{
            
                fList <- list.files(path = inDir, full.names = T, recursive = F)
            
                if(length(fList) != tgtCnt){
                  writeLines(paste0('PROBLEM, ', length(fList), ',', inDir), fileConn)
                  OK <- F
                }else{
                  #writeLines(paste0('OK, ', length(fList), ',', inDir), fileConn)
                }
            }
        }
        
        if(OK){
          writeLines(paste0('OK,',lims[i]), fileConn)
        }
  }
}

close(fileConn)

