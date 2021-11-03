#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 15:23:52 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Fills holes, removes outliers apply masks etc in the raw covariate rasters
#################################


library(raster)
library(doSNOW)

rasterOptions(progress='text')

rootDir <- 'e:/Projects/Roper/Covariates/all'
outDir <-  'e:/Projects/Roper/Covariates/finessed'

#catMaskR <- raster('P:/3_Land_suitability/5_DSM/Boundaries/demMask_WGS84_geo.tif')
raster('E:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')

# 1.	Clim_minann 
r <- raster(paste0(rootDir, '/Clim_minann.tif'))
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_minann.tif'))

# 2.	Clim_minwin.tif
r <- raster(paste0(rootDir, '/Clim_minwin.tif'))
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_minwin.tif'))


#3.	Clim_Prestcott 
r <- raster(paste0(rootDir, '/Clim_PrescottIndex_01_1s_lzw.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, upper=1.2)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Clim_PrescottIndex_01_1s_lzw.tif'))

#4.	Clim_rainan.tif 
r <- raster(paste0(rootDir, '/Clim_rainan.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_rainan.tif'))


#5.	Clim_rainsum
r <- raster(paste0(rootDir, '/Clim_rainsum.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_rainsum.tif'))

#6.	Clim_sum_wint_ratio 
r <- raster(paste0(rootDir, '/Clim_sum_wint_ratio.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, upper=400)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Clim_sum_wint_ratio.tif'))

#7.	Clim_tdayann – values 36 – 85
r <- raster(paste0(rootDir, '/Clim_tdayann.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_tdayann.tif'))

#8. Clim_varan 
r <- raster(paste0(rootDir, '/Clim_varan.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_varan.tif'))

#9. PM_gravity  
r <- raster(paste0(rootDir, '/PM_gravity.tif'))
r
plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,5,5), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
writeRaster(b1, paste0(outDir, '/PM_Gravity.tif'), overwrite=TRUE)


#10.	PM_magnetics   
r <- raster(paste0(rootDir, '/PM_magnetics.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)

######### Fill holes in rasters by using values from surrounding cells   #######################################################

b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
r2 <- clamp(b4, lower=-200, upper=400)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/PM_magnetics.tif'))
file.remove(list.files('c:/temp', full.names = T))


#11.	PM_radmap_v4_2019_K 
r <- raster(paste0(rootDir, '/PM_radmap_v4_2019_filtered_pctk_GAPFilled.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=-0.5, upper=3)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/PM_radmap_v4_2019_filtered_pctk_GAPFilled.tif'))

#	12.	PM_radmap_v4_2019_t
r <- raster(paste0(rootDir, '/PM_radmap_v4_2019_filtered_ppmt_GAPFilled.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=0, upper=40)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/PM_radmap_v4_2019_filtered_ppmt_GAPFilled.tif'))

#	13.	PM_radmap_v4_2019_u
r <- raster(paste0(rootDir, '/PM_radmap_v4_2019_filtered_ppmu_GAPFilled.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=-0.5, upper=5)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/PM_radmap_v4_2019_filtered_ppmu_GAPFilled.tif'))



#14.	PM_silica  
r <- raster(paste0(rootDir, '/PM_silica.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/PM_silica.tif'))

#15.	PM_Weathering_Index
r <- raster(paste0(rootDir, '/PM_Weathering_Index.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/PM_Weathering_Index.tif'))

#16.	Relief_aspect
r <- raster(paste0(rootDir, '/Relief_aspect_1s.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
#plot(b4)
#hist(b4, maxpixels=10000)
writeRaster(b4, paste0(outDir, '/Relief_aspect_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#17.	Relief_dem
r <- raster(paste0(rootDir, '/Relief_demcrop.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Relief_demcrop.tif'))


#18.	Relief_focalmedian300m
r <- raster(paste0(rootDir, '/Relief_focalmedian300m_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=0, upper=15)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_focalmedian300m_1s.tif'))

#19.	Relief_focalrange1000m
r <- raster(paste0(rootDir, '/Relief_focalrange1000m_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=0, upper=150)
#hist(r2, maxpixels=10000)
#plot(r2)
b1 <- focal(r2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
#plot(b4)
#hist(b4, maxpixels=10000)
writeRaster(b4, paste0(outDir, '/Relief_focalrange1000m_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#20.	Relief_focalrange1000m
r <- raster(paste0(rootDir, '/Relief_focalrange300m_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)
r2 <- clamp(b5, lower=0, upper=100)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_focalrange300m_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#21.	Relief_mrrtf6g
r <- raster(paste0(rootDir, '/Relief_mrrtf6g.a5_1s.tif'))
#r
plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)
plot(b5)
writeRaster(b5, paste0(outDir, '/Relief_mrrtf6g.a5_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#22.	Relief_mrvbf_int
r <- raster(paste0(rootDir, '/Relief_mrvbf_int.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)
#plot(b5)
writeRaster(b5, paste0(outDir, '/Relief_mrvbf_int.tif'))
file.remove(list.files('c:/temp', full.names = T))

#23.	Relief_plan_curv
r <- raster(paste0(rootDir, '/Relief_plan_curvature_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
#quantile(r, c(.01,  .99))
r2 <- clamp(r, lower=-0.07479985, upper=0.07312131)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_plan_curvature_1s.tif'))


#24.	Relief_profile_curv
r <- raster(paste0(rootDir, '/Relief_profile_curvature_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
#quantile(r, c(.01,  .99))
r2 <- clamp(r, lower=-0.002086927, upper=0.001927850)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_profile_curvature_1s.tif'))

#25.	Relief_slopeperc
r <- raster(paste0(rootDir, '/Relief_slopepct1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)

hist(b5, maxpixels=10000)
quantile(b5, c(  .99))
r2 <- clamp(b5,  upper=22)
plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_slopepct1s.tif'), overwrite=T)
file.remove(list.files('c:/temp', full.names = T))

#27.	Soil_illite
r <- raster(paste0(rootDir, '/Soil_Illite.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)

writeRaster(b4, paste0(outDir, '/Soil_Illite.tif'))
file.remove(list.files('c:/temp', full.names = T))

#28.	Soil_kaolinite
r <- raster(paste0(rootDir, '/Soil_Kaolinite.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
#quantile(b3, c(.01,  .99))
r2 <- clamp(b3, lower=0.4490363, upper=0.6017709)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/Soil_Kaolinite.tif'))
file.remove(list.files('c:/temp', full.names = T))

#29.	Soil_smectite
r <- raster(paste0(rootDir, '/Soil_Smectite.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
#quantile(b3, c(.01,  .99))
r2 <- clamp(b3, lower=0.1488147, upper=0.3318712)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/Soil_Smectite.tif'))
file.remove(list.files('c:/temp', full.names = T))

#30.	Veg_FPAR_mean
r <- raster(paste0(rootDir, '/Veg_FPAR_Mean.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
writeRaster(b1, paste0(outDir, '/Veg_FPAR_Mean.tif'))

#31.	Veg_FractCover_Mean_BS
r <- raster(paste0(rootDir, '/Veg_FractCover_Mean_BS.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
writeRaster(b4, paste0(outDir, '/Veg_FractCover_Mean_BS.tif'))
file.remove(list.files('c:/temp', full.names = T))


#32.	Veg_FractCover_Mean_PV
r <- raster(paste0(rootDir, '/Veg_FractCover_Mean_PV.tif'))
# r
 plot(r)
 hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
quantile(b4, c(.01,  .99))
plot(b4)
r2 <- clamp(b4, lower=18, upper=52)
plot(r2)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/Veg_FractCover_Mean_PV.tif'))
file.remove(list.files('c:/temp', full.names = T))


#33.	Veg_Persistent_green
r <- raster(paste0(rootDir, '/Veg_Persistant_green_Veg.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)

#quantile(b2, c(.01,  .99))
r2 <- clamp(b2, upper=155)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Veg_Persistant_green_Veg.tif'))
file.remove(list.files('c:/temp', full.names = T))

file.copy(paste0(rootDir, "/Landsat30Bare_1.tif"), paste0(outDir, '/Landsat30Bare_1.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_2.tif"), paste0(outDir, '/Landsat30Bare_2.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_3.tif"), paste0(outDir, '/Landsat30Bare_3.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_4.tif"), paste0(outDir, '/Landsat30Bare_4.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_5.tif"), paste0(outDir, '/Landsat30Bare_5.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_6.tif"), paste0(outDir, '/Landsat30Bare_6.tif'))






###### Apply Mask to all finessed covariates

inDir <- 'e:/Projects/Roper/Covariates/finessed'
outDir <-  'e:/Projects/Roper/Covariates/finessedM'
if(!dir.exists(outDir)){dir.create(outDir)}

catMaskR <- raster('E:/Projects/Roper/Boundaries/bdyBoxWithNaRiver.tif')

fls <- list.files(inDir, '*.tif', full.names = T)
fls

for (i in 1:length(fls)) {
  print(i)
  inF <- fls[i]
  outF <- paste0(outDir, '/', basename(inF))
  r <- raster(inF)
  r <- mask(r, catMaskR, filename=outF)
}


numcpus <- 4
cl <- makeSOCKcluster(numcpus)
registerDoSNOW(cl)
pb <- txtProgressBar(initial=1, min=1, max=length(fls), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
result <- foreach(m=1:length(fls), .packages=c('raster'), .options.snow=opts)  %dopar% { doMask(inDir, outDir, fls)  }
close(pb)
stopCluster(cl)
gc()

doMask<-function(inDir, outDir, fls){
  
  inF <- fls[m]
  outF <- paste0(outDir, '/', basename(inF))
  r <- raster(inF)
  r2 <- mask(r, catMaskR, filename=outF)
  r=NULL
  r2=NULL
  gc()
}



library(raster)
#library(doParallel)
library(doSNOW)

rasterOptions(progress='text')

rootDir <- 'e:/Projects/Roper/Covariates/all'
outDir <-  'e:/Projects/Roper/Covariates/finessed'

#catMaskR <- raster('P:/3_Land_suitability/5_DSM/Boundaries/demMask_WGS84_geo.tif')
raster('E:/Projects/Roper/Boundaries/demMask_WGS84_geo.tif')

# 1.	Clim_minann 
r <- raster(paste0(rootDir, '/Clim_minann.tif'))
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_minann.tif'))

# 2.	Clim_minwin.tif
r <- raster(paste0(rootDir, '/Clim_minwin.tif'))
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_minwin.tif'))


#3.	Clim_Prestcott 
r <- raster(paste0(rootDir, '/Clim_PrescottIndex_01_1s_lzw.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, upper=1.2)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Clim_PrescottIndex_01_1s_lzw.tif'))

#4.	Clim_rainan.tif 
r <- raster(paste0(rootDir, '/Clim_rainan.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_rainan.tif'))


#5.	Clim_rainsum
r <- raster(paste0(rootDir, '/Clim_rainsum.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_rainsum.tif'))

#6.	Clim_sum_wint_ratio 
r <- raster(paste0(rootDir, '/Clim_sum_wint_ratio.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, upper=400)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Clim_sum_wint_ratio.tif'))

#7.	Clim_tdayann – values 36 – 85
r <- raster(paste0(rootDir, '/Clim_tdayann.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_tdayann.tif'))

#8. Clim_varan 
r <- raster(paste0(rootDir, '/Clim_varan.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Clim_varan.tif'))

#9. PM_gravity  
r <- raster(paste0(rootDir, '/PM_gravity.tif'))
r
plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,5,5), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
writeRaster(b1, paste0(outDir, '/PM_Gravity.tif'), overwrite=TRUE)


#10.	PM_magnetics   
r <- raster(paste0(rootDir, '/PM_magnetics.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
r2 <- clamp(b4, lower=-200, upper=400)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/PM_magnetics.tif'))
file.remove(list.files('c:/temp', full.names = T))


#11.	PM_radmap_v4_2019_K 
r <- raster(paste0(rootDir, '/PM_radmap_v4_2019_filtered_pctk_GAPFilled.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=-0.5, upper=3)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/PM_radmap_v4_2019_filtered_pctk_GAPFilled.tif'))

#	12.	PM_radmap_v4_2019_t
r <- raster(paste0(rootDir, '/PM_radmap_v4_2019_filtered_ppmt_GAPFilled.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=0, upper=40)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/PM_radmap_v4_2019_filtered_ppmt_GAPFilled.tif'))

#	13.	PM_radmap_v4_2019_u
r <- raster(paste0(rootDir, '/PM_radmap_v4_2019_filtered_ppmu_GAPFilled.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=-0.5, upper=5)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/PM_radmap_v4_2019_filtered_ppmu_GAPFilled.tif'))



#14.	PM_silica  
r <- raster(paste0(rootDir, '/PM_silica.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/PM_silica.tif'))

#15.	PM_Weathering_Index
r <- raster(paste0(rootDir, '/PM_Weathering_Index.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/PM_Weathering_Index.tif'))

#16.	Relief_aspect
r <- raster(paste0(rootDir, '/Relief_aspect_1s.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
#plot(b4)
#hist(b4, maxpixels=10000)
writeRaster(b4, paste0(outDir, '/Relief_aspect_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#17.	Relief_dem
r <- raster(paste0(rootDir, '/Relief_demcrop.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
writeRaster(r, paste0(outDir, '/Relief_demcrop.tif'))


#18.	Relief_focalmedian300m
r <- raster(paste0(rootDir, '/Relief_focalmedian300m_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=0, upper=15)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_focalmedian300m_1s.tif'))

#19.	Relief_focalrange1000m
r <- raster(paste0(rootDir, '/Relief_focalrange1000m_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
r2 <- clamp(r, lower=0, upper=150)
#hist(r2, maxpixels=10000)
#plot(r2)
b1 <- focal(r2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
#plot(b4)
#hist(b4, maxpixels=10000)
writeRaster(b4, paste0(outDir, '/Relief_focalrange1000m_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#20.	Relief_focalrange1000m
r <- raster(paste0(rootDir, '/Relief_focalrange300m_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)
r2 <- clamp(b5, lower=0, upper=100)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_focalrange300m_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#21.	Relief_mrrtf6g
r <- raster(paste0(rootDir, '/Relief_mrrtf6g.a5_1s.tif'))
#r
plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)
plot(b5)
writeRaster(b5, paste0(outDir, '/Relief_mrrtf6g.a5_1s.tif'))
file.remove(list.files('c:/temp', full.names = T))

#22.	Relief_mrvbf_int
r <- raster(paste0(rootDir, '/Relief_mrvbf_int.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)
#plot(b5)
writeRaster(b5, paste0(outDir, '/Relief_mrvbf_int.tif'))
file.remove(list.files('c:/temp', full.names = T))

#23.	Relief_plan_curv
r <- raster(paste0(rootDir, '/Relief_plan_curvature_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
#quantile(r, c(.01,  .99))
r2 <- clamp(r, lower=-0.07479985, upper=0.07312131)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_plan_curvature_1s.tif'))


#24.	Relief_profile_curv
r <- raster(paste0(rootDir, '/Relief_profile_curvature_1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
#quantile(r, c(.01,  .99))
r2 <- clamp(r, lower=-0.002086927, upper=0.001927850)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_profile_curvature_1s.tif'))

#25.	Relief_slopeperc
r <- raster(paste0(rootDir, '/Relief_slopepct1s.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
b5 <- focal(b4, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b5.tif', overwrite=TRUE)

hist(b5, maxpixels=10000)
quantile(b5, c(  .99))
r2 <- clamp(b5,  upper=22)
plot(r2)
writeRaster(r2, paste0(outDir, '/Relief_slopepct1s.tif'), overwrite=T)
file.remove(list.files('c:/temp', full.names = T))

#27.	Soil_illite
r <- raster(paste0(rootDir, '/Soil_Illite.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)

writeRaster(b4, paste0(outDir, '/Soil_Illite.tif'))
file.remove(list.files('c:/temp', full.names = T))

#28.	Soil_kaolinite
r <- raster(paste0(rootDir, '/Soil_Kaolinite.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
#quantile(b3, c(.01,  .99))
r2 <- clamp(b3, lower=0.4490363, upper=0.6017709)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/Soil_Kaolinite.tif'))
file.remove(list.files('c:/temp', full.names = T))

#29.	Soil_smectite
r <- raster(paste0(rootDir, '/Soil_Smectite.tif'))
#r
#plot(r)
#hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
#quantile(b3, c(.01,  .99))
r2 <- clamp(b3, lower=0.1488147, upper=0.3318712)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/Soil_Smectite.tif'))
file.remove(list.files('c:/temp', full.names = T))

#30.	Veg_FPAR_mean
r <- raster(paste0(rootDir, '/Veg_FPAR_Mean.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
writeRaster(b1, paste0(outDir, '/Veg_FPAR_Mean.tif'))

#31.	Veg_FractCover_Mean_BS
r <- raster(paste0(rootDir, '/Veg_FractCover_Mean_BS.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
writeRaster(b4, paste0(outDir, '/Veg_FractCover_Mean_BS.tif'))
file.remove(list.files('c:/temp', full.names = T))


#32.	Veg_FractCover_Mean_PV
r <- raster(paste0(rootDir, '/Veg_FractCover_Mean_PV.tif'))
# r
 plot(r)
 hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b3.tif', overwrite=TRUE)
b4 <- focal(b3, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b4.tif', overwrite=TRUE)
quantile(b4, c(.01,  .99))
plot(b4)
r2 <- clamp(b4, lower=18, upper=52)
plot(r2)
#hist(r2, maxpixels=10000)
writeRaster(r2, paste0(outDir, '/Veg_FractCover_Mean_PV.tif'))
file.remove(list.files('c:/temp', full.names = T))


#33.	Veg_Persistent_green
r <- raster(paste0(rootDir, '/Veg_Persistant_green_Veg.tif'))
# r
# plot(r)
# hist(r, maxpixels=10000)
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'c:/temp/b2.tif', overwrite=TRUE)

#quantile(b2, c(.01,  .99))
r2 <- clamp(b2, upper=155)
#hist(r2, maxpixels=10000)
#plot(r2)
writeRaster(r2, paste0(outDir, '/Veg_Persistant_green_Veg.tif'))
file.remove(list.files('c:/temp', full.names = T))


outDir <- 'E:/temp/finessed'
r <- raster('E:/temp/RoperClimateCovs/Cropped/Clim_ADM.tif')
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,5,5), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b3.tif', overwrite=TRUE)
writeRaster(b3, paste0(outDir, '/Clim_ADM.tif'))

r <- raster('E:/temp/RoperClimateCovs/CroppedAll/Clim_TNM.tif')
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,5,5), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b3.tif', overwrite=TRUE)
writeRaster(b3, paste0(outDir, '/Clim_TNM.tif'))

r <- raster('E:/temp/RoperClimateCovs/CroppedAll/Clim_PTA.tif')
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,5,5), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b3.tif', overwrite=TRUE)
writeRaster(b3, paste0(outDir, '/Clim_PTA.tif'))

r <- raster('E:/temp/RoperClimateCovs/CroppedAll/Clim_PTRX.tif')
b1 <- focal(r, w=matrix(1,21,21), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b1.tif', overwrite=TRUE)
b2 <- focal(b1, w=matrix(1,5,5), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b2.tif', overwrite=TRUE)
b3 <- focal(b2, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/b3.tif', overwrite=TRUE)
writeRaster(b3, paste0(outDir, '/Clim_PTRX.tif'))

clipArea <- raster('N:/3_Land_suitability/4_Data/covariates/all/Relief_demcrop.tif')
compareRaster(b3, clipArea)






file.copy(paste0(rootDir, "/Landsat30Bare_1.tif"), paste0(outDir, '/Landsat30Bare_1.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_2.tif"), paste0(outDir, '/Landsat30Bare_2.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_3.tif"), paste0(outDir, '/Landsat30Bare_3.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_4.tif"), paste0(outDir, '/Landsat30Bare_4.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_5.tif"), paste0(outDir, '/Landsat30Bare_5.tif'))
file.copy(paste0(rootDir, "/Landsat30Bare_6.tif"), paste0(outDir, '/Landsat30Bare_6.tif'))






###### Apply Mask to all finessed covariates

inDir <- 'e:/Projects/Roper/Covariates/finessed'
outDir <-  'e:/Projects/Roper/Covariates/finessedM'
if(!dir.exists(outDir)){dir.create(outDir)}

catMaskR <- raster('E:/Projects/Roper/Boundaries/bdyBoxWithNaRiver.tif')

fls <- list.files(inDir, '*.tif', full.names = T)
fls

for (i in 1:length(fls)) {
  print(i)
  inF <- fls[i]
  outF <- paste0(outDir, '/', basename(inF))
  r <- raster(inF)
  r <- mask(r, catMaskR, filename=outF)
}


numcpus <- 4
cl <- makeSOCKcluster(numcpus)
registerDoSNOW(cl)
pb <- txtProgressBar(initial=1, min=1, max=length(fls), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
result <- foreach(m=1:length(fls), .packages=c('raster'), .options.snow=opts)  %dopar% { doMask(inDir, outDir, fls)  }
close(pb)
stopCluster(cl)
gc()

doMask<-function(inDir, outDir, fls){
  
  inF <- fls[m]
  outF <- paste0(outDir, '/', basename(inF))
  r <- raster(inF)
  r2 <- mask(r, catMaskR, filename=outF)
  r=NULL
  r2=NULL
  gc()
}





catMaskR <- raster('E:/Projects/Roper/Boundaries/bdyBoxWithNaRiver.tif')
demR <- raster('e:/temp/dem1sv1_0.tif')
raster::resample(demR, catMaskR, method='ngb' , filename='e:/temp/dem1Sec.tif' )

r <- raster('e:/temp/dem1Sec.tif')
r
plot(r)

