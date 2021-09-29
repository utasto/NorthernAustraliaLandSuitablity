
######## About this Script #########################################################################
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:18:04 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : This script controls and monitors suitability processing on the HPC
###            Some of my best work I would have to say.....
###            Controls Mapping of attribute models, processing of Suitability subclasses 
###            and Mapping of overall suitabilities 
##################################################################################################

library(raster)
library(stringr)
library(rgdal)


library(raster)
library(stringr)

ident = 'sea084'
project='Rop'
#att = 'SoilDepth'
jobStartIteration = 1
jobEndIteration = 700
memoryGB = '10GB'
wallTime = '00:10:00'

numLayers = 39

rootDir <- '/datasets/work/lw_rowra/work/3_Land_suitability/0_Working/Uta/Roper'
scriptsPath <- '/datasets/work/af-digiscapesm/work/Ross/Roper/Scripts'
#outPath <- '/datasets/work/af-digiscapesm/work/Ross/Roper/Outputs'
debugPath <- '/datasets/work/af-digiscapesm/work/Ross/Roper/HPCout'

#templateR <- raster(paste0(covDir, '/Relief_demcrop.tif'))
source(paste0(scriptsPath, '/RoperLandSuit/HPC/HPCUtils.R'))
source(paste0(scriptsPath, '/RoperLandSuit/Mapping/RandomForestUtils_Roper.R'))
source(paste0(scriptsPath, '/RoperLandSuit/Suitability/SuitUtils_Roper.R'))


#########   Make Maps ######


att<- 'SoilDepth'
jobEndIteration=1
makeBlockFile(outPath, att, templateR, covDir, rootDir, numLayers = numLayers)

jobFileName <- paste0(scriptsPath, '/LimRasters_', att, '.sh')
tidyUpPreviousRun(outPath, att, debugPath, deleteChunks=F)
makeJobFile(att, jobFileName )

jobID <- sendPredictionJob(project, att, wallTime, memoryGB, jobStartIteration, jobEndIteration, debugPath,jobFileName)


showJobInfo('sea084', 20, 'RUNNING')
showJobInfo('sea084', 20, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')

jobID <- '45347114'

monitorJob(jobID) 

showCPUs()
showQ(ident)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
cancelJob(jobID)
showJobLog(20)

jobFileName <- paste0(scriptsPath, '/makeMap_', att, '.sh')
makeMapJobFile(att, jobFileName, 'Classification')
jobID <- sendMapJob(project=project, att=att, wallTime='00:30:00', memoryGB='1GB', debugPath=debugPath,jobFileName=jobFileName)

moveOutputsToBowen(outPath, att)


modelsToMap <- read.csv(paste0('/datasets/work/af-digiscapesm/work/Ross/Roper', '/ModelPaths.csv'), stringsAsFactors = F) 
atts <- modelsToMap$AttributeName

jobids <- vector(mode = "list", length = 17)

for (i in 15:17) { 
  att <- atts[i]
  makeBlockFile(outPath, att, templateR, covDir, rootDir, numLayers = numLayers)
  jobFileName <- paste0(scriptsPath, '/LimRasters_', att, '.sh')
  tidyUpPreviousRun(outPath, att, debugPath, deleteChunks=T)
  makeJobFile(att, jobFileName )
  
  jobids[i] <- sendPredictionJob(project, att, wallTime, memoryGB, jobStartIteration, jobEndIteration, debugPath,jobFileName)
  print(paste0( att, " : ", jobids[i]))
  cat(as.character(Sys.time()),',Prediction,', att,',', as.character(jobids[i]), '\n', sep='', file=paste0(scriptsPath, '/HPCLog.csv'), append = T)
  
}


#### Check for number of output files
for (i in 1:length(atts)) {
  att <- atts[i]
  dPath <- paste0(outPath, 'Attributes/', att, '/Chunks')
  fls <- list.files(dPath, '*.rds')
  print(paste0(att, '   ', length(fls)))
}

#### generate maps
for (i in 1:length(atts)) {
  att <- atts[i]
  jobFileName <- paste0(scriptsPath, '/makeMap_', att, '.sh')
  makeMapJobFile(att, jobFileName)
  jobID <- sendMapJob(project=project, att=att, wallTime='01:30:00', memoryGB='1GB', debugPath=debugPath,jobFileName=jobFileName)
  print(paste0( att, " : ", jobID))
  cat(as.character(Sys.time()),',Mapping,', att,',', as.character(jobID), '\n', sep='', file=paste0(scriptsPath, '/HPCLog.csv'), append = T)
  moveOutputsToBowen(outPath, att)
}

cancelJob('45168325')
showQ('sea084')










################   Suitability


rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'


#suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V4.xlsx')
suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(rootDir, '/Suitability/LimitationsFileMappingsRoper2.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

useList <- getSuitList(suitFramework)
limsToDo <- unique(suitFramework$LIM_Name[suitFramework$NumQuals==1])

###### Irrigation_efficiency-low_rate_methods  might be problematic


lim <- 'Wetness'
lim <- 'Irrigation_efficiency-high_rate_methods'
lim <- 'Microrelief'
lim <- 'Moisture_availability_1.0'
lim <- 'Moisture_availability_0.6'
lim <- 'Nutrient_balance'
lim <- 'Rockiness'
lim <- 'Surface_Condition'
lim <- 'Surface_Infiltration'
lim <- 'Salinity_surface' # probs
lim <- 'Erosion' 


lim <- 'Moisture_availability_RAINFED_CROPPING_100'  
lim <- 'Climate-temp_variation'
lim <- 'Climate-frost'
lim <- 'Climate-heat_dry_season_and_perennial'
lim <- 'Climate-heat_wet_season_and_perennial'
lim <- 'Climate-annual_rainfall'
lim <- 'Acid_Sulfate_Soil_Potential' 
lim <- 'Soil_depth' 


lim <- 'Surface_Texture' # - ###########3   WARNING - I have hardcoded an exp() function in to the surface texture code to transform from log values in the "calcSubClasses" evaluation function


lim <- 'Irrigation_efficiency-low_rate_methods'  

project='Rop'

######   Send the job off  #######
landuses <- paste(useList, collapse = "=")
#landuses <- paste(useList[c(57,58)], collapse = "=")

jobStartIteration = 1
jobEndIteration = 700

#landuses <- "Mango!per!tri_Mango!per!spr_Citrus!per!tri_Cucurbits!dry!spr_CottonAndGrains!wet!spr_CottonAndGrains!dry!fur_CottonAndGrains!dry!spr_CottonAndGrains!wet!rainfed_Rice-upland!wet!rainfed_PulseCrops!wet-dr!rainfed_Cane!wet-long!fur_Cane!wet-long!spr_Cane!wet-long!rainfed"
jobFileName <- paste0(scriptsPath, '/subclasses_', lim, '.sh')
makeSubclassJobFile(lim, landuses, jobFileName)
tidyUpPreviousRunSubclasses(outPath, lim, debugPath, deleteChunks=F)
jobID <- sendSubclassJob(project=project, lim=lim, wallTime='02:31:00', memoryGB='15GB', jobStartIteration, jobEndIteration, debugPath=debugPath,jobFileName=jobFileName, scriptsPath = scriptsPath)
Sys.sleep(10)
showJobInfo('sea084', 30, 'ALL')

showJobInfo('sea084', 10, 'RUNNING')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'COMPLETED')
showJobInfo('sea084', 20, 'PENDING') 

showDebugFile(jobName = jobName,  )

#######   functions for monitoring progress on the HPC
showQ(ident)

showQforJob(jobID)
showQforJob(55061903)
cancelJob(jobID)


cancelJob(55057235)
monitorJob(jobID) 

showCPUs()
showQ(ident)
tail(showDetails(jobID), 100)
showNonCompletedJobs(55064023)
showFailedJobs(jobID)
cancelJob(jobID)
showJobLog()
showAllUsers()

getTimeTaken(outPath, 'Subclasses' , lim, 'mins')

jobID <- '54345895'
cancelJob(jobID)



######  Calculate Overall suitability
rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
suitFile = paste0(rootDir, '/Suitability/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(rootDir, '/Suitability/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(rootDir, '/Suitability/LimitationsFileMappingsRoper2.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

useList <- getSuitList(suitFramework)
limsToDo <- unique(suitFramework$LIM_Name[suitFramework$NumQuals==1])


#sjid <- 'MapSuits'
landuses <- paste(useList, collapse = "_")
landuses <- "Forage_Sorghum-Maize!wet!rainfed"
jobFileName <- paste0(scriptsPath, '/Suitability/MapSuits.sh')
makeSuitabilityJobFile(landuses, jobFileName, scriptsPath)

jobStartIteration = 1
jobEndIteration = 700
# jName = 'test1'
# jobID <- sendSuitabilityJob(project=project, jName, wallTime='00:10:00', memoryGB='10GB', jobStartIteration, jobEndIteration, debugPath=debugPath,jobFileName=jobFileName, scriptsPath = scriptsPath)


#for (i in 49:58) {
for (i in 1:1) {
  landuse <- useList[i]
  landuse <- "Forage_Sorghum-Maize!wet!rainfed"
  print(landuse)
  jobFileName <- paste0(scriptsPath, '/Suitability/MapSuits_', i, '.sh')
  makeSuitabilityJobFile(landuse, jobFileName, scriptsPath = scriptsPath)
  tidyUpPreviousSuitability(rootDir, att=landuse, debugPath, deleteChunks=T)
  jobID <- sendSuitabilityJob(project=project, landuse, wallTime='00:10:00', memoryGB='5GB', jobStartIteration, jobEndIteration, debugPath=debugPath,jobFileName=jobFileName, scriptsPath = scriptsPath)

  Sys.sleep(150)
  
}

showJobInfo('sea084', 10, 'ALL')



monitorJob('51768489')

showCPUs()
showQ('sea084')
showJobInfo('sea084', 10, 'RUNNING')

showJobInfo('sea084', 10, 'ALL', fromTime = '2020-07-29T11:40')
showJobInfo('sea084', 20, 'FAILED')
cancelJob(jobID)
monitorJob(jobID)

showFailedJobs(jobID)






#######  Map Suitability   


useList <- colnames(suitFramework)[15:ncol(suitFramework)]
uses <- paste(useList, collapse = "_")

#jobStartIteration = 57
jobStartIteration = 43
jobEndIterationMapping = 43

jobFileName <- paste0(scriptsPath, '/SuitabilityMaps/makeMaps_', jobStartIteration, '_', jobEndIterationMapping, '.sh')
makeSuitabilityMapJobFile(uses, jobFileName, scriptsPath)

jobID <- sendSuitabilityMapJob(project=project, wallTime='01:59:00', memoryGB='20GB', jobStartIteration, jobEndIterationMapping, debugPath=debugPath,jobFileName=jobFileName, scriptsPath = scriptsPath)

showJobInfo('sea084', 10, 'ALL')



showQ('sea084')
showJobInfo('sea084', 10, 'RUNNING')

cancelJob('45433084')

jobID <- '45353282'


showAllUsers()
HPCLoad()
