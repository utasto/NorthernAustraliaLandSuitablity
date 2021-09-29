
#################################
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:20:36 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Functions to submit and monitor jobs on the Pearcey HPC
###            At the moment the script that access these functions is "HPC Job Control.R"
#################################

library(raster)
library(raster)
library(stringr)
library(rgdal)
library(stringr)
library(dplyr)


tidyUpPreviousSuitability<- function(outPath, att, debugPath, deleteChunks=F){
  if(deleteChunks){
    unlink(list.files(paste0(outPath, '/Suitability/', att, '/Chunks'), "*.fst", recursive = F, full.names = T))
  }
  unlink(list.files(paste0(debugPath, '/Suitability/', att), recursive = T, full.names = T))
}

tidyUpPreviousRun<- function(outPath, att, debugPath, deleteChunks=F){
  if(deleteChunks){
    unlink(list.files(paste0(outPath, '/Attributes/', att), "*.rds", recursive = F, full.names = T))
  }
  unlink(list.files(paste0(debugPath, '/Predictions/', att), recursive = T, full.names = T))
}

tidyUpPreviousRunSubclasses <- function(outPath, lim, debugPath, deleteChunks=F){
  if(deleteChunks){
    unlink(list.files(paste0(outPath, '/Subclasses/', lim), "*.fst", include.dirs = T, recursive = T, full.names = T))
    unlink(list.dirs(paste0(outPath, '/Subclasses/', lim), recursive = F, full.names = T), recursive = T)
  }
  unlink(list.files(paste0(debugPath, '/Subclasses/', lim), recursive = T, full.names = T))
}


makeJobFile <- function(att, jobFileName){
  sfile <-  jobFileName 
  cat('#!/bin/sh\n', file = sfile)
  cat('module load R/3.6.1\n', file = sfile, append = T)
  cat('/apps/R/3.6.1/lib64/R/bin/Rscript /home/sea084/roper/Scripts/RoperLandSuit/Mapping/RandomForestAttributeMaping_Roper_HPC.R $SLURM_ARRAY_TASK_ID ', att, file=sfile, append = T)
  system(paste0('chmod 755 ', sfile) )
}

makeSuitabilityMapJobFile <- function(landuses, jobFileName, scriptsPath){
  sfile <-  jobFileName 
  cat('#!/bin/sh\n', file = sfile)
  cat('module load R/3.6.1\n', file = sfile, append = T)
  cat('/apps/R/3.6.1/lib64/R/bin/Rscript ', scriptsPath, '/RoperLandSuit/Suitability/makeSuitabilityMapsHPC.R $SLURM_ARRAY_TASK_ID  ', file=sfile, sep = '', append = T)
  system(paste0('chmod 755 ', sfile) )
}

makeSuitabilityJobFile <- function( uses, jobFileName, scriptsPath){
  sfile <-  jobFileName 
  cat('#!/bin/sh\n', file = sfile)
  cat('module load R/3.6.1\n', file = sfile, append = T)
  cat('/apps/R/3.6.1/lib64/R/bin/Rscript ', scriptsPath, '/RoperLandSuit/Suitability/calculateOverallSuitabilityHPC.R $SLURM_ARRAY_TASK_ID ',uses, file=sfile, sep = '', append = T)
  system(paste0('chmod 755 ', sfile) )
}

makeSubclassJobFile <- function(lim, landuses, jobFileName){
  sfile <-  jobFileName 
  cat('#!/bin/sh\n', file = sfile)
  cat('module load R/3.6.1\n', file = sfile, append = T)
  cat('/apps/R/3.6.1/lib64/R/bin/Rscript /home/sea084/roper/Scripts/RoperLandSuit/Suitability/calculateSuitabilitySubclassesHPC.R $SLURM_ARRAY_TASK_ID ', lim, ' ', landuses, file=sfile, sep = '', append = T)
  system(paste0('chmod 755 ', sfile) )
}

makeMapJobFile <- function(att, jobFileName){
  sfile <-  jobFileName 
  cat('#!/bin/sh\n', file = sfile)
  cat('module load R/3.6.1\n', file = sfile, append = T)
  cat('/apps/R/3.6.1/lib64/R/bin/Rscript /home/sea084/roper/Scripts/RoperLandSuit/Mapping/makeMapFromChunks_HPC.R ', att, file=sfile, append = T)
  system(paste0('chmod 755 ', sfile) )
}

monitorJob <- function(jobID){
  f='go'
  while (f != 'stop'){
    cpus <- system('scc', intern = T)
    d <- system(paste0('sacct -j ', jobID), intern = T)
    d2 <- d[which(grepl('h2', d))]
    completed <- which(grepl('COMPLETED', d2))
    msg <- paste0(cpus[2], ' :  ', length(completed), ' of ', (jobEndIteration - jobStartIteration) + 1, ' jobs completed')
    cat("\r", "")
    cat("\r", msg)
    Sys.sleep(10)
  }
}

showJobLog <- function(tail=NULL){
  fname <- paste0(scriptsPath, '/HPCLog.csv')
  df <- read.csv(fname, stringsAsFactors = F)
  if(is.null(tail)){
    return(df)
  }else{
    return(tail(df, tail))
  }
}


showDetails <- function(jobID){
  d <- system(paste0('sacct -j ', jobID), intern = T)
  d2 <- d[which(grepl('h2', d))]
  d3 <-str_trim( gsub("\\s+", " ", d2))
  df <- read.csv(text=d3, sep = ' ', header = F)
  df2 <- df[, c(1,2,4,7,8,9,10,11)]
  colnames(df2) <- c('JobID', 'JobName', 'User', 'Time1', 'Time2', 'Status', 'Memory', 'Node')
  
  return(df2)
}

sendPredictionJob <- function(project, att, wallTime, memoryGB, jobStartIteration, jobEndIteration, debugPath,jobFileName){
  if(!dir.exists(paste0(debugPath, '/Predictions/', att))){dir.create(paste0(debugPath, '/Predictions/', att), recursive = T)}
  job=paste0('sbatch --parsable --job-name=', paste0(project,att),' --time=', wallTime, ' --mem=', memoryGB, ' -a ', jobStartIteration,'-',jobEndIteration,' -o ',debugPath,'/Predictions/', att,'/Pred_out_%a.txt -e ',debugPath,'/Predictions/', att,'/Pred_error_%a.txt ',jobFileName)
  jobID <- system(job, intern = T)
  print(paste0('Prediction : ', att, " : ", jobID))
  cat(as.character(Sys.time()),',Prediction,', att,',', as.character(jobID), '\n', sep='', file=paste0(scriptsPath, '/HPCLog.csv'), append = T)
  return(jobID)
}

sendSuitabilityMapJob <- function(project, wallTime, memoryGB, jobStartIteration, jobEndIteration, debugPath,jobFileName,scriptsPath){
  if(!dir.exists(paste0(debugPath, '/SuitabilityMaps'))){dir.create(paste0(debugPath, '/SuitabilityMaps'), recursive = T)}
  
  
  ## -w c018
  
  job=paste0('sbatch --parsable --job-name=', paste0(project,'RSMap'),' --time=', wallTime, ' --mem=', memoryGB, ' -a ',jobStartIteration,'-',jobEndIteration, ' -o ',debugPath,'/SuitabilityMaps', '/Pred_out_%a.txt -e ',debugPath,'/SuitabilityMaps', '/Pred_error_%a.txt ', jobFileName)
  jobID <- system(job, intern = T)
  print(paste0('Suitability : ', 'SuitabilityMap', " : ", jobID))
  cat(as.character(Sys.time()),',SuitabilityMap,', ',', as.character(jobID), '\n', sep='', file=paste0(scriptsPath, '/HPCLog.csv'), append = T)
  
  return(jobID)
}

sendSuitabilityJob <- function(project, jName, wallTime, memoryGB, jobStartIteration, jobEndIteration, debugPath,jobFileName,scriptsPath){
  if(!dir.exists(paste0(debugPath, '/Suitability/',jName))){dir.create(paste0(debugPath, '/Suitability/',jName), recursive = T)}
 # jobFileName <- paste0(scriptsPath, '/subclasses_', lim, '.sh')
  job=paste0('sbatch --parsable --job-name=', paste0(project,'S' , jName),' --time=', wallTime, ' --mem=', memoryGB, ' -a ',jobStartIteration,'-',jobEndIteration, ' -o ',debugPath,'/Suitability/',jName, '/', jName, '_Pred_out_%a.txt -e ',debugPath,'/Suitability/',jName, '/', jName, '_Pred_error_%a.txt ', jobFileName)
  print(job)
  jobID <- system(job, intern = T)
  print(paste0('Suitability  : ', jobID))
  of <- paste0(scriptsPath, '/HPCLog.csv')
  if(!file.exists(of)){cat("startTime,SobType,JobName,JobID", '\n', sep='', file=paste0(scriptsPath, '/HPCLog.csv'), append = F)}
  cat(as.character(Sys.time()),',Suitability,', jName, ',', as.character(jobID), '\n', sep='', file=paste0(of), append = T)
  
  return(jobID)
}

sendSubclassJob <- function(project, lim, wallTime, memoryGB, jobStartIteration, jobEndIteration, debugPath,jobFileName,scriptsPath){
  if(!dir.exists(paste0(debugPath, '/Subclasses/', lim))){dir.create(paste0(debugPath, '/Subclasses/', lim), recursive = T)}
  jobFileName <- paste0(scriptsPath, '/subclasses_', lim, '.sh')
  job=paste0('sbatch --parsable --job-name=', paste0(project,'Sub', lim),' --time=', wallTime, ' --mem=', memoryGB, ' -a ',jobStartIteration,'-',jobEndIteration, ' -o ',debugPath,'/Subclasses/', lim, '/Pred_out_%a.txt -e ',debugPath,'/Subclasses/', lim, '/Pred_error_%a.txt ', jobFileName)
  jobID <- system(job, intern = T)
  print(paste0('Subclasses : ', lim, " : ", jobID))
  of <- paste0(scriptsPath, '/HPCLog.csv')
  if(!file.exists(of)){cat("startTime,JobType,JobName,JobID", '\n', sep='', file=paste0(scriptsPath, '/HPCLog.csv'), append = F)}
  cat(as.character(Sys.time()),',Subclasses,', lim,',', as.character(jobID), '\n', sep='', file=paste0(of), append = T)
  
  return(jobID)
}

sendMapJob <- function(project, att, wallTime, memoryGB, debugPath,jobFileName){
  if(!dir.exists(paste0(debugPath, '/MakeMaps'))){dir.create(paste0(debugPath, '/MakeMaps'))}
  
  job <- paste0('sbatch --parsable --job-name=', paste0(project,'M',att),' --time=', wallTime, ' --mem=', memoryGB, ' -o ', debugPath, '/MakeMaps/', att, '_Map_out.txt -e ', debugPath, '/MakeMaps/', att, '_Map_error.txt ', jobFileName)
  jobID <- system(job, intern = T)
  return(jobID)
}

moveOutputsToBowen <- function(outPath, att){
  
  file.copy(paste0(outPath, '/Attributes/', att, '/', att, '.tif'), paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Limitations/', att, '.tif'), overwrite = T )
  file.copy(paste0(outPath, '/Attributes/', att, '/', att, '_Uncert.tif'), paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Limitations/', att, '_Uncert.tif'), overwrite = T)
}

makeBlockFile <- function(outPath, att, templateR, covDir, rootDir, numLayers){
  
  outRasterDir <- paste0(outPath , 'Attributes/', att)
  chunkRasterDir <- paste0(outRasterDir, '/Chunks')
  if(!dir.exists(chunkRasterDir)){dir.create(chunkRasterDir, recursive = T)}
  bsFile <- (paste0(chunkRasterDir, '/blocks.dat'))
  if(file.exists(bsFile)){
    unlink(bsFile)
  }
  
  bs <- blockSize(templateR, n=numLayers)
  saveRDS(bs, bsFile)
  return(bsFile)
}


showCPUs <- function(){
  return(system('scc'))
}

showQ <- function(ident){
  system (paste0('squeue -u ', ident))
}

showQforJob <- function(jobID){
  system (paste0('squeue -j ', jobID))
}


showNonCompletedJobs <- function(jobID){
  r <- showDetails(jobID)
  r2 <- r[which(!grepl('COMPLETED', r$Status)), ]
  return(r2)
}

showFailedJobs<- function(jobID){
  r <- showDetails(jobID)
  r2 <- r[which(grepl('FAILED', r$Status)), ]
  return(r2)
}

cancelJob <- function(jobID){
  system (paste0('scancel ', jobID))
}






showJobInfo <- function(ident, tnum=10, filter="ALL", fromTime=''){
  
  if(nchar(fromTime)>0 ){fromTime = paste0(' -S ', fromTime, ' ') }
  # limit returned records to date  -S 2020-07-28T10:00:00
  d <- system(paste( 'sacct -u ', ident, fromTime, ' -P -X -o jobid,start,end,state%30'), intern = T)
  df <- read.csv(text=d, sep = '|', header = T, stringsAsFactors = F)
  
  allids <- str_split(df$JobID, '_')
  alljobids <- as.character(sapply(allids, function(x) x[1]))
  alltaskids <- as.character(sapply(allids, function(x) x[2]))
  df4 <- df[, -1]
  df3 <- data.frame(JobID=alljobids, TaskID=alltaskids, stringsAsFactors = F)
  df2 <- cbind(df3, df4)
  
  jobIDs <- unique(df2$JobID)
  jidsToDo <- tail(jobIDs, tnum)

   sdf <-  data.frame(JOBID=character(), PENDING=character(), COMPLETED=character(), FAILED=character(), 
                     RUNNING=character(), CANCELLED=character(), TIMEOUT=character(),OUT_OF_MEMORY=character(),stringsAsFactors = F)
   
   
  for (i in 1:length(jidsToDo)) {
    
    jid <- jidsToDo[i]
    jobfil <- df2[which(grepl(jid, df2$JobID)),]
    cnts <- jobfil %>% group_by(State) %>% tally()

    #cnts <- as.data.frame(jobfil %>% count(State))
    p <- cnts[cnts$State=='PENDING',2]
    if(nrow(p)==0){p=0}
    c1 <- cnts[cnts$State=='COMPLETED',2]
    if(nrow(c1)==0){c1=0}
    f <- cnts[cnts$State=='FAILED',2]
    if(nrow(f)==0){f=0}
    r <- cnts[cnts$State=='RUNNING',2]
    if(nrow(r)==0){r=0}
    c2 <- cnts[str_detect(cnts$State, 'CANCELLED'),2]
    if(nrow(c2)==0){c2=0}
    t <- cnts[str_detect(cnts$State, 'TIMEOUT'),2]
    if(nrow(t)==0){t=0}
    m <- cnts[str_detect(cnts$State, 'OUT_OF_MEMORY'),2]
    if(nrow(m)==0){m=0}
    sdf <- sdf %>% add_row(JOBID=jid, PENDING=p, COMPLETED=c1, FAILED=f, RUNNING=r, CANCELLED=c2, TIMEOUT=t,OUT_OF_MEMORY=m )
  }
  
  jl <- showJobLog(tnum)
  
  mdf <- merge(sdf, jl, by.x = 'JOBID', by.y = 'JobID')
  of <- str_to_upper(filter)
  if(!filter=='ALL'){
    odf3 <- mdf[which(mdf[of] > 0),]
    return(odf3)
  }else{
    return(mdf)
  }
}


numberOfRulesForLimitation <- function(lim , suitFramework){
  return(length(suitFramework$LIM_Name[suitFramework$LIM_Name==lim]))
}


countOutputFiles <- function(outPath, type, att, numUses, numFiles){

    inDir <- paste0(outPath, '/', type, '/', att)
    dirs <- list.dirs(inDir, full.names = F, recursive = F)
    
    fCount <- vector(mode = 'numeric', numUses)
    subgrp <- vector(mode = 'character', numUses)
    complete <- vector(mode = 'logical', numUses)
   
      for(j in 1:length(dirs)){
        cat('=',sep='')
        d <- dirs[j]
        subDir <- paste0(inDir, '/', d)
        fls <- list.files(subDir, '*.fst', recursive = T, full.names = T)
        #print(paste0(d, ' = ' ,length(fls)))
        fCount[j] = length(fls)
        subgrp[j] = d
        complete[j] = (length(fls)==numFiles)
      }
    cat('\n',sep='')
    df <- data.frame(att, subgrp, fCount, complete, stringsAsFactors = F)
  return(df)
}

taskProcessingTimes <- function(outPath, type, att){
  
  inDir <- paste0(outPath, '/', type, '/', att)
  dirs <- list.dirs(inDir, full.names = F, recursive = F)
  
  id <- vector(mode = 'numeric', 608)
  fCount <- vector(mode = 'numeric', 608)
  stime <- vector(mode = 'character', 608)
  etime <- vector(mode = 'character', 608)
  mins <- vector(mode = 'numeric', 608)
  
  fls <- list.files(inDir, '*.fst', recursive = T, full.names = F)
  
  for (i in 1:608) {
    print(i)
    filt <- paste0('_', i, '.fst')
    b <- fls[which(grepl(filt, fls))]
    fi <- file.info(paste0(inDir, '/', fls))
    dif <- max(fi$mtime) - min(fi$mtime)
    t <- as.numeric(dif, units='mins')
    
    id[i] <- i
    fCount[i] <- length(fls)
    stime[i] <- as.character(min(fi$mtime))
    etime[i] <- as.character(max(fi$mtime))
    mins[i] <- t
  }
  
  df <- data.frame(att, id, fCount, stime, etime, mins, stringsAsFactors = F)
  return(df)
}


getTimeTaken <- function(outPath, type, att, units){
  fls <- list.files(paste0(outPath, '/', type, '/', att), '*.fst', recursive = T, full.names = T)
  fi <- file.info(fls)
  dif <- max(fi$mtime) - min(fi$mtime)
  t <- as.numeric(dif, units=units)
  return(t)
}

showAllUsers <- function(){
  d <- system(paste0('squeue'), intern = T)
  
  d3 <-str_trim( gsub("\\s+", " ", d))
  df <- read.csv(text=d3, sep = ' ', header = T)
  cnts <- as.data.frame(df %>% count(USER))
  
  ocnts <- cnts[order(-cnts$n),]
  return(ocnts)
 
}
