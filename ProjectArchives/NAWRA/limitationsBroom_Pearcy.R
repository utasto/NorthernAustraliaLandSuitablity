


TESTING <- F


if(TESTING == T){
  rootDirSweeps = 'D:/temp/sweeptest'
  rootDir = 'D:/Projects/NAWRA/Production'
}else{
  rootDirSweeps = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Pearcy/LimitationSweeps'
  rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
  rootDirUnix ='/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production'
  rootDirSweepsUnix = '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production/Pearcy/LimitationSweeps'
}

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')

cats <- c('Mitchell', 'Fitzroy')
#cats <- c('Darwin', 'Mitchell', 'Fitzroy')
#cats <- c( 'Darwin')
#lims <- c('Climate-annual_rainfall')
#lims <- c('Climate-heat_dry_season_and_perennial')
#lims <- c('Climate-heat_wet_season_and_perennial')
#lims <- c('Climate-frost')
lims <- c('Climate-temp_variation')
#lims <- c('Wetness')
#lims <- c('Moisture_availability_1.5')
#lims <- c('Moisture_availability_1.0')
#lims <- c('Moisture_availability_0.6')
#lims <- c('Nutrient_balance')
#lims <- c('Soil_depth')
#lims <- c('Rockiness')
#lims <- c('Microrelief')
#lims <- c('Irrigation_efficiency')
#lims <- c('Irrigation_efficiency-high_rate_methods')
#lims <- c('Moisture_availability_RAINFED_CROPPING_15')
#lims <- c('Moisture_availability_RAINFED_CROPPING_10')
#lims <- c('Moisture_availability_RAINFED_CROPPING_06')
#lims <- c('Acid_Sulfate_Soil_Potential')
#lims <- c('Erosion')
#lims <- c('Salinity_surface')
#lims <- c('Surface_Condition')
#lims <- c('Surface_Texture')
#lims <- c('Surface_Infiltration')


landuses <- "Alluses"


lims <- c('Climate-annual_rainfall', 'Climate-heat_dry_season_and_perennial', 'Climate-heat_wet_season_and_perennial', 'Climate-frost','Climate-temp_variation',
          'Wetness','Moisture_availability_1.5', 'Moisture_availability_1.0', 'Moisture_availability_0.6', 
          'Nutrient_balance', 'Soil_depth', 'Rockiness', 'Microrelief', 'Irrigation_efficiency', 'Irrigation_efficiency-high_rate_methods',
          'Moisture_availability_RAINFED_CROPPING_15', 'Moisture_availability_RAINFED_CROPPING_10','Moisture_availability_RAINFED_CROPPING_06',
          'Acid_Sulfate_Soil_Potential', 'Erosion', 'Salinity_surface',
          'Surface_Condition', 'Surface_Texture','Surface_Infiltration')

limcodes <- c('Cr', 'Chw', 'Chd', 'Cf','Ctv',
          'W','M15', 'M10', 'M06', 
          'Nu', 'Sd', 'Ro','Mi', 'Ie', 'Iehr',
          'Mr15', 'Mr10','Mr06',
          'Ass', 'Er', 'Sa',
          'SC', 'ST','SI')

limdf <- data.frame(lims, limcodes)

catNames <- c('Darwin', 'Mitchell', 'Fitzroy')
catCodes <- c('D', 'M', 'F')

catdf <- data.frame(catNames, catCodes)

#####   One node per landuse sweep generation

for (i in 1:length(cats)){
  
  catcode <- as.character(catdf[catdf$catNames == cats[i],2])
  suitfile <- paste0(cats[i], '_Suitability_Framework.xlsx')
  sheet <- 'Subclasses'
  suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  uses <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
  usesstring <-  paste(uses, collapse=";") 
  
  
  fnameSweeper.sh <- paste0(rootDirSweeps, '/',cats[i], '_AllSweeps.sh')
  unlink(fnameSweeper.sh)
  fileConnSweeper<-file(fnameSweeper.sh, open = 'wb')
  writeLines('#!/bin/bash', fileConnSweeper)
  
  for(j in 1:length(lims)){
    
    limcode <- as.character(limdf[limdf$lims == lims[j],2])
    outDir <- paste0(rootDirSweeps, '/', cats[i], '/', lims[j])
    outDirUnix <- paste0(rootDirSweepsUnix, '/', cats[i], '/', lims[j])
    dir.create(outDir, recursive = T)
    
    outDirErr <- paste0(outDir, '/errOut')
    outDirErrUnix <- paste0(outDirUnix, '/errOut')
    dir.create(outDirErr, recursive = T)    

    writeLines(paste0('sbatch ', cats[i], '/', lims[j], '/',lims[j], '.sh'), fileConnSweeper)
    
    curLims <- suitFramework[suitFramework$LIM_Name==lims[j],]
    
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
    
    fnameUses <- paste0(outDir, '/Uses4Lim_',lims[j], '.csv')
    write.csv(useList, fnameUses)
    
        fname <- paste0(outDir, '/',lims[j], '.sh')
        unlink(fname)
        fileConn<-file(fname, open = 'wb')
        
        writeLines('#!/bin/bash', fileConn)
        writeLines(paste0('#SBATCH --job-name="',catcode, 'L', limcode, '"'), fileConn)
        writeLines('#SBATCH --nodes=1', fileConn)
        writeLines('#SBATCH --ntasks=20', fileConn)
        writeLines('#SBATCH --time=3:00:00', fileConn)
        writeLines('#SBATCH --mem=12GB', fileConn)
        writeLines('#SBATCH --exclusive', fileConn)
        writeLines(paste0('#SBATCH -a 1-', length(useList)), fileConn)
        writeLines(paste0('#SBATCH -o ', outDirErrUnix, '/out_%a.txt'), fileConn)
        writeLines(paste0('#SBATCH -o ', outDirErrUnix, '/error_%a.txt'), fileConn)
        
        writeLines('module load R/3.4.0', fileConn)
        writeLines(paste0('Rscript ', rootDirUnix, '/Scripts/Subclasses/calculateSuitabilitySubclassesHPC3.R ', cats[i], ' ', lims[j], ' $SLURM_ARRAY_TASK_ID'), fileConn)
        
        close(fileConn)
        print(paste0(lims[j]))
  }
  close( fileConnSweeper)
}




















##### batch files for Grouped landuses
cnt<-1
for (i in 1:length(cats)){

  suitfile <- paste0(cats[i], '_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
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
