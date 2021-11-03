
#################################
###  Author : Ross Searle         
###  Date : Thu Sep 30 12:35:03 2021                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Plot all the suitability maps for a visual inspection
#################################


library(raster)

dataDir <- '/datasets/work/af-digiscapesm/work/Ross/Roper/Suitability/Maps'

fls <- list.files(dataDir, pattern = '_m.tif', full.names = T)
idxs <- which('Uncert' %in% fls)
fls2 <- fls[grep("Uncert",fls,invert=TRUE)]


stk <- stack(fls2[1:16])
stk <- stack(fls2[17:32])
stk <- stack(fls2[33:48])
stk <- stack(fls2[49:56])
plot(stk, breaks=cuts, col = pal(5), legend=F)



r <- raster(fls[1])

cuts=c(0,1,2,3,4,5)
pal <- colorRampPalette(c("green","yellow", "brown"))
#plot(r, maxpixels = 10000, col=terrain.colors(5))

plot(r, maxpixels = 100000,breaks=cuts, col = pal(5))
