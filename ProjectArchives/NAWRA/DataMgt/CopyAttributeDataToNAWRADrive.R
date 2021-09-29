library(readxl)

pInfo <- as.data.frame(read_excel('V:/Projects/NAWRA/Production/Scripts/DataMgt/Copy_NTdata.xlsx', col_names = T))
pInfo <- as.data.frame(read_excel('V:/Projects/NAWRA/Production/Scripts/DataMgt/Copy_QLDdata.xlsx', col_names = T))
pInfo <- as.data.frame(read_excel('V:/Projects/NAWRA/Production/Scripts/DataMgt/Copy_WAdata.xlsx', col_names = T))

for( i in 1:nrow(pInfo)) {
  
  srcFile <- paste0(pInfo$`Source Directory`, pInfo$`Source Name`)
  destFile <- paste0(pInfo$`Destination Directory`, pInfo$`Destination Name`)
  
  if(file.exists(srcFile[i])){
    
   destDir <- dirname(destFile[i])
   if(!dir.exists(destDir)){
     dir.create(destDir)
   }
   print(paste0("copying file - ", srcFile[i]))
   file.copy(srcFile[i], destFile[i])
    
  }else{
    print(paste0("ERROR #### File doesn't exist - ", srcFile[i]))
  }
}
