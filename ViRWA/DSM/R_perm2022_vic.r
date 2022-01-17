#17 Jan 2022

library(raster)
library(rgdal)
library(ranger)
library(caret)
#set.seed(10)

#read data
R_data_dir = "/datasets/work/lw-rowra/work/3_Land_suitability/2_Victoria/0_Working/Uta/Victoria/Obs_Data"
vic.data <- read.csv(paste0(R_data_dir, "/", "ViWRA_DSM_attrib_v2.csv"))
#0 have been removed from coords
names(vic.data)
str(vic.data)
vic.data$Permeability <- factor(vic.data$Permeability)
levels(vic.data$Permeability)
table(vic.data$Permeability)
#1 2 3 4
#280 1430 2302 612

##read covariates (37)
R_covs_dir ="/datasets/work/lw-rowra/work/3_Land_suitability/2_Victoria/0_Working/Uta/Victoria/Covariates/Parsimonious_30m_masked"
filesdat <- list.files(R_covs_dir, pattern="*.tif$",full.names=T) 
filesdat

 for (i in 1:length(filesdat)){
   plot(raster(filesdat[i]), main=(filesdat[i]))
 }

covs1 <- stack(lapply(filesdat, raster))
covs1

plot(raster(covs1, 1))

# create spatialPoints df
coordinates(vic.data) <- c("o_longitude_WGS84", "o_latitude_WGS84")
projection(vic.data)= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

points(vic.data, col="black")

vic.covs <-as.data.frame(extract(covs1,vic.data,method="simple"))
dim(vic.covs)
#first time covs are extracted so save
saveRDS(vic.covs, paste0(R_covs_dir, "/", "vic.covs1.rds"))
##vic.covs <- readRDS(paste0(R_covs_dir, "/", "vic.covs1.rds"))

### create df for modelling all options
vic.perm <- data.frame(vic.data[1:3], vic.data$Year_desc, vic.data$For_DSM, vic.data@coords, vic.data$Permeability, vic.covs)
str(vic.perm)
levels(vic.perm$proj_code)

### modelling
fmla <- as.formula(paste("roper.data.Permeability ~", paste(names(roper.covs), collapse="+")))
fmla

# use caret train to find best mtry
roper.perm <- na.omit(roper.perm)
model <- train(fmla, data = roper.perm, method = "ranger")
#names(model)
model$results #best kappa=0.3420, mytry 21
model$bestTune

perm.mod.1 <- ranger(fmla, data= roper.perm, mtry=21, importance = "permutation", 
                     splitrule= "extratrees", keep.inbag=T, write.forest=T)
perm.mod.1 #OOB pred err 32.96% 
barplot(sort(perm.mod.1$variable.importance), horiz=T, las=1, main="perm.mod.1")
levels(perm.mod.1$predictions)
table(perm.mod.1$predictions, roper.perm$roper.data.Permeability)
confusionMatrix(perm.mod.1$confusion.matrix) #kappa=0.3753
saveRDS(perm.mod.1, "/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/dsm2021/dsm2021/perm.mod.1.rds")

### post 2001
library(tidyverse)
#names(roper.perm)
#head(roper.perm$roper.data.o_date_desc)
#roper.perm$date <- str_sub(roper.perm$roper.data.o_date_desc,start=-4) #year only, -6 for %m%y
#head(roper.perm$date)
#r.perm.2001 <- roper.perm[which(roper.perm$date >= 2001),]
#dim(r.perm.2001)

#new selection as o_date_desc is missing and replaced by Year_desc
names(roper.perm)
head(roper.perm$roper.data.Year_desc)
roper.perm$date <- str_sub(roper.perm$roper.data.Year_desc) #year only, -6 for %m%y
head(roper.perm$date)
r.perm.2001 <- roper.perm[which(roper.perm$date >= 2001),]
dim(r.perm.2001)

#caret selection
r.perm.2001 <- na.omit(r.perm.2001)
model <- train(fmla, data = r.perm.2001, method = "ranger")
#names(model)
model$results #best kappa=0.4055
model$bestTune ##mytry 21

perm.mod.2 <- ranger(fmla, data= r.perm.2001, mtry=21, importance = "permutation", 
                     splitrule= "extratrees", keep.inbag=T, write.forest=T)
perm.mod.2
#OOB= 25.43%
sort(perm.mod.2$variable.importance)
barplot(sort(perm.mod.2$variable.importance), horiz=T, las=1, main="perm.mod.2")
confusionMatrix(perm.mod.2$confusion.matrix) #kappa=0.4183
saveRDS(perm.mod.2, "/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/dsm2021/dsm2021/perm.mod.2.rds")

### yes and maybe yes
levels(roper.perm$roper.data.For_DSM)
r.perm.y <- roper.perm[which(roper.perm$roper.data.For_DSM=="YES"
                             |roper.perm$roper.data.For_DSM=="yes"
                             |roper.perm$roper.data.For_DSM=="MAYBE YES"),]
dim(r.perm.y)

#caret selection
r.perm.y <- na.omit(r.perm.y)
model <- train(fmla, data = r.perm.y, method = "ranger")
#names(model)
model$results #best kappa=0.3824
model$bestTune ##mytry 40

perm.mod.3 <- ranger(fmla, data= r.perm.y, mtry=40, importance = "permutation", 
                     splitrule= "extratrees", keep.inbag=T, write.forest=T)
perm.mod.3
#OOB= 29.29%
sort(perm.mod.3$variable.importance)
barplot(sort(perm.mod.3$variable.importance), horiz=T, las=1, main="perm.mod.3")
confusionMatrix(perm.mod.3$confusion.matrix) #kappa=0.4037
saveRDS(perm.mod.3, "/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/dsm2021/dsm2021/perm.mod.3.rds")

#### ROWRA & RPDSM only
levels(roper.perm$proj_code)
r.perm.r <- roper.perm[which(roper.perm$proj_code=="ROWRA"
                             |roper.perm$proj_code=="RPDSM"),]
dim(r.perm.r) #448 52
#much smaller dataset

##caret selection
r.perm.r <- na.omit(r.perm.r)
model <- train(fmla, data = r.perm.r, method = "ranger")
#names(model)
model$results #best kappa=0.4125
model$bestTune ##mytry 2, not much difference in accuracy, better to use simpler model with less variables per split when performance measures are similar

perm.mod.4 <- ranger(fmla, data= r.perm.r, mtry=2, importance = "permutation", 
                     keep.inbag=T, write.forest=T)
perm.mod.4
#OOB= 33.48%
sort(perm.mod.4$variable.importance)
barplot(sort(perm.mod.4$variable.importance), horiz=T, las=1, main="perm.mod.4")
confusionMatrix(perm.mod.4$confusion.matrix) #kappa=0.3943
saveRDS(perm.mod.4, "/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/dsm2021/dsm2021/perm.mod.4.rds")



















