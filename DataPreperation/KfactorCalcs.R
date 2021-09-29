
#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 10:46:20 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : PTF for calculating the K Factor use in the Erosion limitation 
#################################


library(readxl)

####   The sensible range of values for K (Metric) is between 0.01 and 0.88  #######



calc_K_Factor <- function(oc=1,pedGrade='M', pedType = 'SB', pedSize=2, clay, silt, sandFine, sandCoarse, perm=3){

USLE_K_OM <- mapply(calc_OC, oc)
USLE_K_SS <- mapply(calc_SS, pedGrade, pedType, pedSize)
USLE_K_PR <- mapply(calc_Perm, perm)

USLE_K_P125 <- (silt +  (0.7 * sandFine)) * (silt + sandFine + sandCoarse)     
USLE_K_RAW = (2.77 * 10^-7 * USLE_K_P125 ^ 1.14 * (12 - USLE_K_OM)) + (4.28 * 10^-3 * (USLE_K_SS - 2)) + (3.29 * 10^-3 * (USLE_K_PR - 3))
USLE_K_ADJ = USLE_K_RAW / (0.4621+0.048*(1.03259^(sandFine+sandCoarse)))

USLE_K_P125b <-  (clay + silt + (0.7 * sandFine)) * (100 * exp((-0.019) * clay))
USLE_K_RAWb = (2.77 * 10^-7) * USLE_K_P125b^ 1.14 * (12 - USLE_K_OM) + 4.28 * 10^-3 * (USLE_K_SS - 2) + 3.29 * 10^-3 * (USLE_K_PR - 3)
USLE_K_ADJb = USLE_K_RAWb / ((1.462 + (0.048 * 1.03259^sandFine + sandCoarse)) - 1)

df <- data.frame(oc, pedGrade, pedType, pedSize, clay, silt, sandFine, sandCoarse, perm, USLE_K_ADJ)
return (df)

}

calc_Perm<- function(perm){
  if(perm == 1)
    return(6)
  if(perm == 2)
    return(5)
  if(perm == 3)
    return(4)
  if(perm == 4)
    return(2)
 
  return(4)
}

calc_OC<- function(oc){
  return ( min(4, (oc * 1.72))) 
}

calc_SS <- function(pedGrade, pedType, pedSize){
  if(pedGrade == 'V'){
    return (4)
  }
  if(pedType %in% c('PL', 'SB', 'AB', 'CO')){
    return (4)
  }
  if(pedSize >= 2){
    return (3)
  }
  if(pedSize == 1){
    return (2)
  }
  return (2)
}




infile <- 'C:/Projects/Roper/SoilData/qry_Kfactor_export2021.xlsx'
d <- as.data.frame(read_excel(infile, col_names = T))
inData <- data.frame(d$OC, d$str_ped_grade, d$str_ped_type, d$str_ped_size, d$Clay, d$Silt, d$Sand_Fine, d$Sand_Coarse, d$Permeability, stringsAsFactors = FALSE)


d$str_ped_size[is.na(d$str_ped_size)] <- 2
d$str_ped_grade[is.na(d$str_ped_grade)] <- 'M'
d$str_ped_type[is.na(d$str_ped_type)] <- 'SB'
d$Permeability[is.na(d$Permeability)] <- 4
d$OC[is.na(d$OC)] <- 1

k <- calc_K_Factor(oc = d$OC, pedGrade=d$str_ped_grade, pedType = d$str_ped_type, pedSize = d$str_ped_size, clay = d$Clay, silt = d$Silt, sandFine = d$Sand_Fine, sandCoarse = d$Sand_Coarse, perm = d$Permeability)
hist(k$USLE_K_ADJ, breaks=30)

d$K <- k$USLE_K_ADJ
write.csv(d, 'C:/Projects/Roper/SoilData/kfactors.csv')






