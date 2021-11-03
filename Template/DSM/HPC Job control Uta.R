
######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:18:04 2020                      
###  Project : TERN Landscapes
###  Purpose : This script controls and monitors processing on the HPC
###  
###############################################################################################

library(raster)
library(stringr)
library(rgdal)

source(paste0('/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/HPCUtils.R'))
ident = 'sto275'
debugPath <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/HPCout'
#workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts'
workingDir<- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/Scripts'
args=''

#jobName='DemoRscript'
#args='ASCDemo asc.mod.2.rds 20'
#jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:10:00', memoryGB='1GB', jobStartIteration=1, jobEndIteration=10, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

#IMPORTANT####### new argument for NUMERIC cat data only (T), use F, if not applicable, will be ignored if it is a regression tree (continuous data)
jobName='doPredictionsUta'
args= 'sal2_2 sal2.mod.2.rds 20 F'
args= 'sal2_2w sal2.mod.2w.rds 20 F'

jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:20:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=700, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

##A should be A1 for model 1 depth of A
#53932436 sal2_2
#53928025 sal2_2w

showNonCompletedJobs(53915532)
showDetails(53914910)

 
showQ(ident)

monitorJob(jobID, debugPath)
showCPUs2(ident)
showCPUs(ident=ident)
#showCPUs(jobID=jobID)

showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
nsj <- showNonSuccessfullJobs(jobName, debugPath) ###looks for line in output finished successfully

tail(showJobLog(debugPath), 100)
showFailedJobs(jobID)
showFailedJobNos(jobID)

showJobInfo('sto275', 100, 'ALL')
showJobInfo('sto275', 30, 'RUNNING')
showJobInfo('sto275', 20, 'FAILED')
showJobInfo('sto275', 20, 'CANCELLED')
showJobInfo('sto275', 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration =5)
showDebugFile(jobName = jobName, type='out', iteration = 5)

cancelJob(jobID)
cancelJob('51204256')


showAllUsers()
HPCLoad()





