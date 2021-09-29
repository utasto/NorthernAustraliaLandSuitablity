##### General Functions

createEmptyDirectory<-function(dirPath)
  {
  if(!file.exists(dirPath))
  {
    dir.create(dirPath, recursive=T)
    Sys.sleep(1)
  }else{
    
    unlink(dirPath, recursive = T, force = T)
    Sys.sleep(1)
    dir.create(dirPath, recursive=T)
   # f <- list.files(dirPath, full.names = T)
   # file.remove(f)
  }
}

createDirectory<-function(dirPath)
{
  if(!file.exists(dirPath))
  {
    dir.create(dirPath, recursive=T)
    Sys.sleep(1)
  }
}