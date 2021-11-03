library(terra)
library(sf)
library(tictoc)

terraOptions(progress=10)

rootDir <- 'E:/Projects/NthAustSuit'
#project <- 'SoGWRA'
project <- 'ViWRA' 
  
box <- st_read(paste0(rootDir, '/', project, '/Boundaries/Box_', project, '.shp'))
plot(box)  
  

fls <- list.files('M:/work/datasets/national/covariates/mosaics/30m', pattern = '.tif$', full.names = T)


for (i in 1:length(fls)) {
  
  f <- fls[i]
  print(basename(f))
  outPath <- paste0(rootDir, '/',project, '/Covariates/', basename(f))
  if(!file.exists(outPath)){
    inR <- rast(f)
    ccr <- terra::crop(inR, box, filename=outPath)
  }
}
