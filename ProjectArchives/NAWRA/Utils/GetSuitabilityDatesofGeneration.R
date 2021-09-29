rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Suitability'

CatchmentName <- 'Fitzroy'
dirs <- list.dirs(paste0(rootDir, '/', CatchmentName), full.names = T, recursive = F)


fnames <- vector(length(dirs), mode = "character" )
dates <- vector(length(dirs), mode = "character" )


for(i in 1:length(dirs)){
  
  f <- dirs[i]
  f2 <- paste0(f, '/chunks/suit_100.rds')
  print(paste0(basename(dirs[i]), '  --  ' , as.character(file.info(f2)$ctime)))
  
  fnames[i] <- basename(dirs[i])
  dates[i] <- as.character(file.info(f2)$ctime)
}
df <- data.frame(name = fnames, date = as.Date(dates))
df[order(df$date),] 

