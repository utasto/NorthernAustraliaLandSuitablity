library(stringr)

rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
destDir <- 'e:/NAWRA'

fs <- list.files(paste0(rootDir, '/Attributes'), pattern = '.tif$', full.names = T, recursive = T)
fs <- list.files(paste0(rootDir, '/Suitability/Fitzroy'), pattern = '.tif$', full.names = T, recursive = T)

for(i in 1:length(fs)){
  f<-fs[i]
  toFile <- str_replace(f, rootDir, destDir)
  print(toFile)
  dir.create(dirname(toFile), recursive = T)
  file.copy(f, toFile)
}