library(raster)

rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
newMaskPath <-paste0(rootDir, '/Attributes/AWC150/Fitzroy/AWC150_Fitzroy.tif')
outname <- paste0(rootDir, '/Masks/template_Fitzroy.tif')

newMaskPath <-paste0(rootDir, '/Attributes/AWC150/Darwin/AWC150_Darwin.tif')
outname <- paste0(rootDir, '/Masks/template_Darwin.tif')

newMaskPath <-paste0(rootDir, '/Attributes/AWC150/Mitchell/AWC150_Mitchell.tif')
outname <- paste0(rootDir, '/Masks/template_Mitchell.tif')

r1 <- raster(newMaskPath)
r2 <- r1/r1
rf <- writeRaster(r2, filename=outname, datatype='INT4S', overwrite=TRUE)


rt <- raster(outname)
rt
plot(rt)