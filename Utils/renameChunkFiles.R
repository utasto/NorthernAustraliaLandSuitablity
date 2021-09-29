#################################
###  Author : Ross Searle         
###  Date : Wed Sep 29 10:51:30 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Rename the DSM output chunks to match the suitability code requirements
#################################


library(stringr)


fls <- list.files('/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/Roper/Maps/Slope/Chunks', full.names = T)


for (i in 2:length(fls)) {
  f <- fls[i]
  file.rename(f, str_replace(f, 'r_', 'AllCellVals_') )
}
