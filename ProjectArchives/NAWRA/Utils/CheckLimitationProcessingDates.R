
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')


rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
lims <- readSuitLimitations(rootDir,classFile = 'LimitationRanges.xlsx')

catcmentName <- 'Fitzroy'

print(paste0(catchmentName, ' limitation processing times'))
print(paste0('======================================================'))
for(i in 1: length(lims)){
  lim <- lims[i]
  inDir <- paste0(rootDir, '/Limitations/', catcmentName, '/', lim)
  
  if(file.exists(inDir)){
    
    dirs <- list.dirs(inDir, full.names = F, recursive = F)
    limFile <- paste0(inDir,'/',dirs[1], '/Chunks/subclasses_1.rds')
    print(paste0(lim, ' processed on ', format(file.mtime(limFile),"%A %b %d"), ' at ',  format(file.mtime(limFile))))
    
  }
  else{
    print(paste0("Limitation ", lim, ' does not exist'))
  }
}