library(raster)
library(rgdal)
library(ranger)
library(stringr)
library(tictoc)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+10, overwrite=TRUE) # maxmemory = max no of cells to read into memory

##monitor memory

#####################################################################
#########                                                  ##########
#########  Functions above. Actual processing starts here  ##########
#########                                                  ##########
#####################################################################

args = commandArgs(trailingOnly=T)
tic()

machineName <- as.character(Sys.info()['nodename'])

  if(machineName=='TERNSOIL2'){
    rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
    k=1
    att = 'SGG2b1w'
    modelFileName = 'sgg.mod.2.b2.rds'
    chunkLines = 20
    source('/datasets/work/lw-slga/work/Projects/Roper/Scripts/RFUtils.R')  
  
}else{
  rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
  k = as.numeric(args[1])
  att = args[2]
  modelFileName = args[3]
  chunkLines = as.numeric(args[4])
  isnumeric = as.logical(args[5])
  source('/datasets/work/lw-slga/work/Projects/Roper/Scripts/RFUtils.R')
}

###'Q:/3_Land_suitability/0_Working/Uta/Roper'  
  
###check if still relevant, related to RF model generated



cat(paste0('Iteration No. = ', k, '\n'))

outChkDir <- paste0(rootDir, '/Maps/', att, '/Chunks')
if(!dir.exists(outChkDir)){dir.create(outChkDir, recursive = T)}

outfile <- paste0(rootDir, '/Maps/', att, '/Chunks/cellSummaries_', k, '.rds')
if ( !file.exists(outfile)){

  cat("\nLoading Data.....\n")
  cat("-------------------------\n")
		model <- readRDS(paste0(rootDir, '/Models/', modelFileName))
		templateR <- raster(paste0(rootDir, '/Templates/template.tif'))
		cat('Template\n')
    cat('------------------\n')
		print(templateR)

		cat("\nTemplate successfully loaded\n")

		cat('Covariates\n------------\n')
		cat(paste0(rootDir, '/Covariates/', model$forest$independent.variable.names, '.tif', collapse = '\n'))
		theStack <- stack(paste0(rootDir, '/Covariates/', model$forest$independent.variable.names, '.tif'))
		cat(paste0('\nNumber of covariates being used = ', nlayers(theStack), '\n'))

		cat('\n')
		cat('Loading covariate data ...\n')

		#chk <- blockSize(theStack)
	  chk <-	getChunkInfo(chunkLines, nrow(templateR))

		ncells = chk$nlines[k] * ncol(templateR)

		theSeq = seq(ncells)
		covs = data.frame(theSeq)

		start_time <- Sys.time()
		print(start_time)

		for (i in 1:nlayers(theStack))
		{
		  rl = raster(theStack, layer=i)
		  v <- as.numeric(getValues(rl, row=chk$starts[k], nrows=chk$nlines[k]))

		  #### this is a hack to deal with NA needs to be sorted
		  if(i > 1){
		    badInds <- which(is.na(v))
		    if(length(badInds) > 0)
		    {
		      v[badInds] <- 0
		    }
		  }
		  covs[names(rl)] <- v
		}

		valsi <- which(!is.na(covs[,2]))
		valsnd <- which(is.na(covs[,2]))


		tt <- Sys.time() - start_time

		cat('Covariate data successfully loaded in ', tt, ' seconds\n\n')


		cat('Running model predictions on raster chunk .....\n')
		cat("--------------------------------------------------\n")

		vname <- paste0(rootDir, '/Maps/', att, '/Chunks/cellSummaries_', k, '.rds')
		cat('Data will be written to ', vname, '\n')


		if(length(valsi) > 0){

		  prediction = predict(model, covs[valsi,-1],  predict.all = T)$predictions

		  if(model$forest$treetype == 'Classification')
		  {
		    if(isnumeric)
		    {
		      m <- matrix(prediction,nrow=nrow(prediction))
		      outM <- apply(m,2,convertToNumeric)
		    }else{
		      outM <- prediction
		    }
		  }else{
		    outM <- prediction
		  }

		  predsPlusCellID <- cbind(valsi, outM)
		  
		  ###  Added the 2 lines below back in to save the whole prediction tree
		  bname = paste0(outChkDir, '/AllCellVals_' , k, '.rds')
		  saveRDS(predsPlusCellID, bname)

		  cat('Model predictions run successfully\n\n')

		  ##### summarise the cell data

		  chunkVals <- predsPlusCellID
		  inds <- chunkVals[,1]

		  if(model$forest$treetype == 'Classification'){

		    sumData <- apply(chunkVals[,-1], 1, getCategoricalPredictionSummary)

		  } else if(model$forest$treetype == 'Regression'){

		    sumData <- apply(chunkVals[,-1], 1, getContinuousPredictionSummary)
		  }

		  b <- do.call(rbind, sumData)
		  sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))

		  vname <- paste0(rootDir, '/Maps/', att, '/Chunks/cellSummaries_', k, '.rds')
		  cat(paste0('Summary file = ' , vname, '\n\n'))
		  saveRDS(sumDataDF2, vname)

toc()
		  cat(paste0('Iteration Finished Successfully'))

}
}else{

  cat('\nFile exists - ', paste0('cellSummaries_', k, '.rds'))
  cat(paste0('Iteration Finished Successfully'))
}

