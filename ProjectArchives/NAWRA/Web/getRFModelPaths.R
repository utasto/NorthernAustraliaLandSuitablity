library(XML)

#filesList <- list.files('e:/www/NAWRA/Attributes', pattern = '.html', recursive = T, full.names = T)

filesList <- list.files('v:/Projects/NAWRA/Production/Attributes', pattern = '.html', recursive = T, full.names = T)



ln <- length(filesList)

att=character(ln)
cat=character(ln)
path=character(ln)

for(i in 1:ln){
  
  fname = filesList[i]
  src <- htmlTreeParse(fname, useInternalNodes=TRUE)
  tags <- xpathApply(src, "//div[@id='model-statistics']", xmlAttrs)
  n <- xmlSerializeHook(getNodeSet(src, "//div[@id='model-statistics']//pre/code"))
  s1 <- str_split(n[[1]], "Path = ")
  s2 <- str_split(s1[[1]][2], '&#13;')
  modPath <- s2[[1]][1]
  
  path[i] <- basename(modPath)
  att[i] <- str_split(fname, '/')[[1]][5]
  cat[i] <- str_split(fname, '/')[[1]][6]
  print(modPath)
  
}


df <- data.frame(att, cat, path)

write.csv(df, '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/attributes/RFmodelPathsNew77.csv')
