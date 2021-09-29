
#################################
###  Author : Ross Searle         
###  Date : Thu Sep 30 09:12:21 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Climate and Relief Masking - not much else to say really
#################################

library(raster)

fls <- list.files('N:/3_Land_suitability/0_Working/Uta/Roper/ClimateAndRelief', full.names = T)
templateR <- raster('N:/3_Land_suitability/0_Working/Uta/Roper/Maps/k1/k1.tif')

for(i in 1:length(fls)){
  print(i)
  r <- raster(fls[i])
  mask(r, templateR, filename = paste0('N:/3_Land_suitability/0_Working/Uta/Roper/ClimateAndReliefMasked/', basename(fls[i])))
}


