
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 11:48:14 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Generates a distance to water raster from a polyline for the Aquaculture suitability
#################################

library(raster)
library(stringr)
library(rgdal)

library(raster)
library(sf)
library(reticulate)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


l <- st_read('C:/Projects/Roper/Aquaculture/DistToCoast/coastm.shp')
ls = l[!st_is_empty(l),,drop=FALSE]
plot(ls)

templateR <- raster('C:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')

coast <- rasterize(ls, templateR,field='id', background = 10, filename='C:/Projects/Roper/Aquaculture/DistToCoast/coast.tif')

r1 <- coast
plot(r1)
r1[r1==10] <- NA


# r1utm <- projectRaster(r1, crs = 'EPSG:3857')
# writeRaster(r1utm, filename = 'C:/Projects/Roper/Aquaculture/DistToCoast/coastUTM.tif', overwrite=T )

#d <- distance(r1)
###### ended up using the proximity tool in QGIS as raster function is v slow
###  had to convert to UTM run proximity and then convert back to geographic in QGIS

d2Coast <- raster('C:/Projects/Roper/Aquaculture/DistToCoast/distToWaterGeo.tif')
m <- raster('C:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')

d2m <- mask(d2Coast,m)
plot(d2m)
writeRaster(d2m, 'C:/Projects/Roper/Aquaculture/DistToCoast/distToWaterGeoMasked.tif')
