library(raster)
library(doParallel)

source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Scripts/Utils/SuitabilityUtils_V2.R')

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

lim <- 'Climate-heat_dry_season_and_perennial'
catchmentName <- 'Darwin'


limDir <- paste0(rootDir, '/Limitations/',catchmentName, '/',  lim)
usePathList <- list.dirs(path = limDir, full.names = TRUE, recursive = F)

bs  <- readRDS(paste0(rootDir, '/Masks/Chunks_' , catchmentName,'.rds'))
templateR <- raster(paste0(rootDir, '/Masks/template_' , catchmentName,'_geo.tif'))


# for(i in 1:length(usePathList)){
#   i=1
#   doMapSubClasses(rootDir = rootDir, limitation = lim, usePath = usePathList[i], templateR = templateR, bs=bs)
# }


print("Starting multi threading...")
cl<-makeCluster(numcpus,outfile="")
registerDoParallel(cl)
foreach(i=1:length(usePathList), .packages=c('raster')) %dopar%  doMapSubClasses(rootDir = rootDir, limitation = lim, usePath = usePathList[i], templateR = templateR, bs=bs)
stopCluster(cl)


r <- raster('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Limitations/Darwin/Climate-heat_dry_season_and_perennial/Cassava!wet!spr/Cassava!wet!spr$Climate-heat_dry_season_and_perennial$subclass.tif')
plot(r)
