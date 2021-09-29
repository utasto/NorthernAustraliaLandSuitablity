library(raster) 
library(rgdal)
library(data.table)
library(reshape2)


source('//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/scripts/suitability/suitAnalysisUtils.R')

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'

#catchmentName <- 'Mitchell'
#catchmentName <- 'Fitzroy'
catchmentName <- 'Darwin'

catBdy <- readOGR(dsn='//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Boundaries', layer='Fitzroy_LSbdy94')

validSitesPts <- readOGR(dsn=paste0(rootDir, '/ValidationTrips/Darwin'), layer='Valid_sitesNT')
#validSitesPts <- readOGR(dsn=paste0(rootDir, '/ValidationTrips/Mitchell'), layer='Valid_sitesQld')
#validSitesPts <- readOGR(dsn=paste0(rootDir, '/ValidationTrips/', catchmentName), layer='Valid_sitesWA')
plot(catBdy)
points(validSitesPts)


AllUses <- as.data.frame(read_excel(paste0(rootDir, '/SuitabilityFramework/PriorityLanduses.xlsx'), sheet='PriorityUses', col_names = T))
#AllUses <- AllUses[AllUses$Fitzroy== 'Asian-veg!dry!tri',]
attributeNamesRaw <- readAttributeFileMapppings()
attributeNames <- unique(data.frame(Catchment=attributeNamesRaw$Catchment, FrameworkName=attributeNamesRaw$FrameworkName, ChunkName=attributeNamesRaw$ChunkName))
atts <- attributeNames[attributeNames$Catchment == catchmentName & attributeNames$ChunkName != '???',   ]


  for(i in 1:length(validSitesPts)){
    
    fname <- paste0(validSitesPts@data$Day[i], '_', validSitesPts@data$F1[i], '.csv')
    print(paste0(fname, ': Lon = ', validSitesPts@coords[i,1], ' Lat = ', validSitesPts@coords[i,2]))
    allsuitData <- getSuitValues(rootDir=rootDir, lon=validSitesPts@coords[i,1], lat=validSitesPts@coords[i,2], catchmentName=catchmentName, landuses=AllUses[catchmentName], attributeNames = atts, doAllLims=F )
    
    
    dfLoc <- allsuitData[[1]]
    dflims <- allsuitData[[2]]
    dfAtts <- allsuitData[[3]]
    dfsuit <- allsuitData[[4]]

    dt <- setDT(dflims)
    dflimsT <- as.data.frame(dcast(dt,Landuse~Limitation, value.var=c( 'Subclass_Value')))
    
    o <- merge(dflimsT, dfsuit, by.x = "Landuse", by.y = "LandUse")
    outTable <- o[,c('Landuse',
                    'Suitability',
                    'Confusion_Index',
                    'Climate-annual_rainfall',
                    'Climate-heat_dry_season_and_perennial',
                    'Climate-heat_wet_season_and_perennial',
                    'Climate-frost',
                    'Climate-temp_variation',
                    'Erosion',
                    'Wetness',
                    'Moisture_availability_1.5',
                    'Moisture_availability_1.0',
                    'Moisture_availability_0.6',
                    'Nutrient_balance',
                    'Soil_depth',
                    'Rockiness',
                    'Microrelief',
                    'Surface_Condition',
                    'Surface_Texture',
                    'Surface_Infiltration',
                    'Irrigation_efficiency',
                    'Irrigation_efficiency-high_rate_methods',
                    'Salinity_surface',
                    'Acid_Sulfate_Soil_Potential',
                    #'Moisture_availability_RAINFED_CROPPING_06',
                    'Moisture_availability_RAINFED_CROPPING_10'
    )]
    

    my.file <- paste0( rootDir,  '/ValidationTrips/', catchmentName, '/SiteDrills/', fname)
    cat('Site ID, ', fname,"\n",file= my.file,append=F)
    cat('Longitude,', validSitesPts@coords[i, 1],"\n",file= my.file,append=T)
    cat('Latitude,', validSitesPts@coords[i, 2],"\n",file= my.file,append=T)
    
    cat("\n" ,file= my.file,append=T)
    cat('Attribute, Value, Uncertainty\n',file= my.file,append=T)
    write.table(dfAtts, row.names = F, col.names = F,file= my.file,append=T, sep=',', na = "")
    
    cat("\n" ,file= my.file,append=T)
    write.table(outTable, row.names = F, col.names = T,file= my.file,append=T, sep=',', na = "")
    
    
  }





