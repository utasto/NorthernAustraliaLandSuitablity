
#################################
###  Author : Ross Searle         
###  Date : Fri Jan 10 11:12:21 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Massages the SRTM 30m DEM data to generate DEM related covariate stack
#################################



library(raster)
library(rgdal)
library(fasterize)
library(leaflet)


# extract and clip 30m AUSLIG DEM to Roper catchment
dem30s <- raster('E:/temp/DEM/dem1sv1_0.tif')
bdy <- readOGR('E:/Roper/DataMassage/roper_srtm_derived_catchment_wgs84.shp')
#rext <- extent(bdy)


#these Supplied by Seonaid on the 9/1/20
Top <- -12.894
Bottom <- -16.778
Left <- 131.995
Right <- 135.7075
rext <- extent(Left, Right, Bottom, Top ) 
rext

# extract and clip 30m SRTM DEM to Roper catchment - we used this as the base DEM

dem30sh <- raster('U:/national/dem/SRTM_derived/1sec/V1_0_official_release/DEM-H/Mosaic/demh1sv1_0/hdr.adf')
c1h  <- crop(dem30sh, rext, snap='out' , filename='E:/Roper/DataMassage/DEM/demcrop.tif')
plot(c1h)
lines(bdy)

rmH <- raster::mask(c1h, bdy)
plot(rmH)
lines(bdy)

riverMask <- readOGR('E:/Roper/DataMassage/river_mask.shp')
lines(riverMask)
rmHR <- raster::mask(rmH, riverMask, inverse=T)
plot(rmHR)
writeRaster(rmHR, filename='E:/Roper/DataMassage/DEM/demMask_WGS84_geo.tif')




# Grab the SRTM Derivative tiles, mosaic them and resample to covariate template

lats <- c(13,14,15,16,17)
lons <- c(131,132,133,134,135)



templateR <- raster('E:/Roper/DataMassage/DEM/demcrop.tif')
outRoot <- 'E:/Roper/DataMassage/DEM'

rootDir <- 'U:/national/dem/SRTM_derived/1sec_terrain_attributes'
atts <- list.dirs(rootDir, recursive = F, full.names = F)

mosPaths <- c('U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_h_twi/mosaic/twi_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_aspect/mosaic/aspect_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_focalrange300m/mosaic/focalrange300m_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_focalrange1000m/mosaic/focalrange1000m_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_plan_curvature/mosaic/plan_curvature_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_prescott_index/mosaic/PrescottIndex_01_1s_lzw.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_profile_curvature/mosaic/profile_curvature_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_slope_relief/mosaic/slope_relief.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_slope_rlf_tpi_class/mosaic/tpi_class_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_slope_rlf_tpi_mask/mosaic/tpi_mask_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_slopedeg/mosaic/slopedeg_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_slopepct/mosaic/slopepct1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/dem_s_slopepct_focalmedian300m/mosaic/focalmedian300m_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/mrrtf6g-a5/mosaic/mrrtf6g-a5_1s.tif',
              'U:/national/dem/SRTM_derived/1sec_terrain_attributes/mrvbf6g-a5/mosaic/mrvbf_int.tif'
)
methods <- c('bilinear', 'bilinear', 'bilinear', 'bilinear', 'bilinear', 'bilinear', 'bilinear', 'ngb', 'ngb', 'ngb', 'bilinear', 'bilinear', 'bilinear', 'bilinear', 'ngb')

for(a in 1:length(mosPaths)){
    print(mosPaths[a])
    outPath <- paste0( outRoot, '/', basename(mosPaths[a]))
    if(!file.exists(outPath)){
        
        mos <- raster(mosPaths[a])
        c1  <- crop(mos, rext, snap='out', filename=paste0( outRoot, '/crp/', basename(mosPaths[a])))
        
        
       # rr <- resample(c1, templateR, filename=  , methods[a])
    }
}


fls <- list.files(paste0( outRoot, '/crp'), '*.tif', full.names = T,)
for(a in 1:length(fls)){
    print(fls[a])
    outPath <- paste0( outRoot, '/', basename(fls[a]))
    if(!file.exists(outPath)){
        
        crp <- raster(fls[a])
        #c1  <- crop(mos, rext, snap='out', filename=paste0( outRoot, '/', basename(fls[a])))
         rr <- resample(crp, templateR, filename=paste0( outRoot, '/', basename(fls[a]))  , methods[a])
    }
}


# tiles don't exist for all surfaces so reverting to mosaics

# for(a in 1:length(atts)){
#     att <- atts[a]
#     dmRoot <- 'E:/Roper/DataMassage/DEM'
#     outDir <- paste0(dmRoot,'/demDerivs/', att)
#     if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
#     for(y in 13:17){
#       for(x in 131:135){
#         fPath <- paste0(rootDir, '/', att, '/tiles/e', x, '/s', y, '/e', x, 's', y, '/hdr.adf')
#         print(paste0('Copying ', fPath))
#         r <- raster(fPath) 
#         writeRaster(r, paste0(outDir, '/', x, '_', y, '.tif'), overwrite=T)
#       }
#     }
#     
#     fls <- list.files(outDir, full.names = T)
#     rl<- vector("list", length(fls))
#     for(i in 1:length(fls)) { 
#       rl[[i]] <- raster(fls[i])
#     }
#     
#     rl$fun <- mean
#     rl$na.rm <- TRUE
#     mos <- do.call('mosaic', rl)
#     plot(mos)
#     lines(bdy)
#     
#     rr <- resample(mos, templateR)
#     writeRaster(rr, paste0( dmRoot, '/', att, '.tif' ))
#     plot(rr)
# 
# }

