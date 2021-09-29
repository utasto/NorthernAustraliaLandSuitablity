

rootDir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Limitations/darwin/Moisture_availability_0.6'
Dir = '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production/Limitations/darwin'


d <- list.dirs(Dir, full.names = F, recursive = F)

for(j in 17:length(d)){

  f <- list.files(paste0(Dir,'/', d[j]), full.names = T, recursive = T)
  print(d[j])
    for(i in 1: length(f)){
      fname = f[i]
      
        if (str_detect(fname, '_122.rds')   ){
          if (str_detect(fname,'subclasses')   ){
              r <- readRDS(fname)
              print(i)
              ids <- which(r[]==-1)
              if(length(ids) > 0){
                print(paste0(length(ids), ' - ', fname))
              }
          }
       }
    }
}