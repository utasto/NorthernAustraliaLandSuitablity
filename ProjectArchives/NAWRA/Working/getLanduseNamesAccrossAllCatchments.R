
source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/mappingUtils.R')
rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'


suitfile <- paste0('Darwin_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
usesDarwin <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

suitfile <- paste0( 'Mitchell_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
usesMitchell <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')

suitfile <- paste0( 'Fitzroy_Suitability_Subclasses_single_sheet_10-05-17.xlsx')
sheet <- 'SUBCLASSES-2-M-linked'
suitFramework <- readSuitFramework(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')
usesFitzroy <- readSuitLanduses(rootDir = rootDir, suitFile = suitfile, sheet = sheet, classFile = 'LimitationRanges.xlsx')


df<- data.frame(Darwin=usesDarwin, Mithell=usesMitchell, Fitzroy=usesFitzroy, stringsAsFactors = F)

write.csv(df, paste0(rootDir,'/working/landuseNames.csv' ))
