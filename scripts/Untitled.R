library(foreach)
library(doParallel)
library(tcltk)
library(sp)
library(raster)

cores<- 2
cl <- makeCluster(cores, output="") #output should make it spit errors
registerDoParallel(cl)


multicore.tabulate.intersect<- function(cores, polygonlist, rasterlayer){ 
  foreach(i=1:cores, .packages= c("raster","tcltk","foreach"), .combine = rbind) %dopar% {
    
    mypb <- tkProgressBar(title = "R progress bar", label = "", min = 0, max = length(polygonlist[[i]]), initial = 0, width = 300) 
    
    foreach(j = 1:length(polygonlist[[i]]), .combine = rbind) %do% {
      final<-data.frame()
      tryCatch({ #not sure if this is necessary now that I'm using foreach, but it is useful for loops.
        
        single <- polygonlist[[i]][j,] #pull out individual polygon to be tabulated
        
        dir.create (file.path("c:/rtemp",i,j,single@data$OWNER), showWarnings = FALSE) #creates unique filepath for temp directory
        rasterOptions(tmpdir=file.path("c:/rtemp",i,j, single@data$OWNER))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of polygons
        
        clip1 <- crop(rasterlayer, extent(single)) #crop to extent of polygon
        clip2 <- rasterize(single, clip1, mask=TRUE) #crops to polygon edge & converts to raster
        ext <- getValues(clip2) #much faster than extract
        tab<-table(ext) #tabulates the values of the raster in the polygon
        
        mat<- as.data.frame(tab)
        final<-cbind(single@data$OWNER,mat) #combines it with the name of the polygon
        unlink(file.path("c:/rtemp",i,j,single@data$OWNER), recursive = TRUE,force = TRUE) #delete temporary files
        setTkProgressBar(mypb, j, title = "number complete", label = j)
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #trycatch error so it doesn't kill the loop
      
      return(final)
    }  
    #close(mypb) #not sure why but closing the pb while operating causes it to return an empty final dataset... dunno why. 
  }
}




myoutput <- multicore.tabulate.intersect(cores, polygonlist, rasterlayer)

myoutput <- multicore.tabulate.intersect(1, polygonlist, rasterlayer)


spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons

plot(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$outlines)

cellFromPolygon(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons)

str(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons, 1)




extent(spatial.dat$`r2_a2_only labels_mask`$RASTERS$CD20_Dy161)
extent(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$maskraster)
extent(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons)
extent(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$outlines)


spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$maskraster


str(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons, 2)
str(spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons@polygons, 2)

x <- spatial.dat$`r2_a2_only labels_mask`$MASKS$cell_mask$polygons
x[1,]


extent(x[1,])







str(x,3)

y <- x[[2]]
str(y, 2)
extent(y)


z <- rasterize(y)

extent(z)

a <- crop(spatial.dat$`r2_a2_only labels_mask`$RASTERS$CD20_Dy161, 
          extent())

plot(a)


