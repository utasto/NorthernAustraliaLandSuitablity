library(raster)
library(rasterVis)
library(ranger)
library(knitr)
library(RColorBrewer)
library(randomcoloR)
library(rmarkdown)
library(rgdal)
library(doParallel)
library(readxl)


modelName <- 'sal2.mod.2w'
mapName <- 'sal2_2w'
maxpixels <- 100000

rootDir <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper'
templatePath <-  '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/ModelReportTemplate.Rmd'
srcPath <- '/datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/RandomForestUtils_V3.R'

rootDir <- 'X:/work/3_Land_suitability/0_Working/Uta/Roper'
templatePath <-  'X:/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/ModelReportTemplate.Rmd'
srcPath <- 'X:/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/RandomForestUtils_V3.R'

ReportModelSummary(rootDir, modelName, mapName, maxpixels )

###########
##run function first
ReportModelSummary <- function(rootDir, modelName, mapName, maxpixels ){
  
  outDir <- paste0(rootDir, '/Reports')
  if(!dir.exists(outDir)){dir.create(outDir)}
  outfile <- paste0(outDir, '/ModelReport_', mapName)
  
  ps = list(  modelName = modelName, mapName = mapName, rootDir = rootDir, maxpixels = maxpixels, srcPath=srcPath)
  #rmarkdown::render(templatePath, params = ps, output_format = 'pdf_document', output_file = paste0(outfile, '.pdf'), quiet=T)
  rmarkdown::render(templatePath, params = ps, output_format = 'html_document', output_file = paste0(outfile, '.html'), quiet=T)
  #rmarkdown::render(templatePath, params = ps, output_format = 'word_document', output_file = paste0(outfile, '.docx'), quiet=T)
  
  # png(file =  paste0(outDir, '/', attribute, '_', catchment,  ".png"), bg = "white", width = 1000, height = 1000)
  # r1 <- raster(paste0(rootDir, '/', attribute, '_', catchment, '.tif'))
  # plot(r1, main = paste0(attribute, ' - ', catchment))
  # dev.off()
  # 
  # png(file =  paste0(outDir, '/', attribute, '_', catchment,  "_Uncert.png"), bg = "white", width = 1000, height = 1000)
  # r1 <- raster(paste0(rootDir, '/', attribute, '_', catchment, '_Uncert.tif'))
  # plot(r1, main = paste0('Cov for ', attribute, ' - ', catchment))
  # dev.off()
  
  if(file.exists(paste0(outfile, '.html')))
    file.show( paste0(outfile, '.html'))
  
}
