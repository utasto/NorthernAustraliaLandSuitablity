#################################
###  Author : Ross Searle         
###  Date : Thu Sep 30 12:36:30 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Reproject the geographic suit maps to UTM for area calculations
#################################


library(raster)

srcDir <- paste0('C:/Projects/Roper/Suitability/Maps')
destDir <- paste0('C:/Projects/Roper/Suitability/MapsUTM')

fls <- list.files(srcDir, full.names = T)

for (i in 1:length(fls)) {
  f <- fls[i]
  print(basename(f))
  cmd <- paste0('C:/LocalProgs/QGIS3.2/bin/gdalwarp.exe -t_srs EPSG:32753 -r near -co compress=DEFLATE -dstnodata 0 -ot UInt16 -of GTiff ', f, ' ', destDir, '/', basename(f) )
  system(cmd )
}
