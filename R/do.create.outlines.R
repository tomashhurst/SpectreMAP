#' do.create.outlines
#'
#' @import data.table
#'
#' @export

#pypath = "/Library/Frameworks/GDAL.framework/Programs/gdal_polygonize.py"

do.create.outlines <- function(spatial.dat,
                               mask.name,
                               use.gdal = FALSE
                               #g.dal.path = NULL
){

  ### Setup
  #message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

  require(raster)
  require(tiff)
  require(rgeos)
  require(tidyr)
  require(ggplot2)

  # polygons.name <- paste0(mask.name, "_polygons")
  # outlines.name <- paste0(mask.name, "_outlines")
  # centroids.name <- paste0(mask.name, "_centroids")

  ### Slow or fast version

  if(use.gdal == TRUE){
    if(length(Sys.which("gdal_polygonize.py")) > 1){
      message(paste0("Creating polygons, outlines, and centroids using GDAL -- this step may take some time, please be patient"))
    }
  }

  if(use.gdal == FALSE){
    message(paste0("Creating polygons, outlines, and centroids using standard method -- this step may take some time, please be patient"))
  }

  ### Run

  for(i in names(spatial.dat)){
    # i <- names(spatial.dat)[[1]]
    start.time <- Sys.time()

    # i <- names(spatial.dat)[[1]]
    mask <- spatial.dat[[i]]$MASKS[[mask.name]]$maskraster

    message(paste0("Processing masks for ROI ", i))

    ## Older andslower version
        if(use.gdal == FALSE){
          polygon <- rasterToPolygons(mask, dissolve=TRUE) # This is the long step
          spatial.dat[[i]]$MASKS[[mask.name]][["polygons"]] <- polygon
          message("... polygons complete")
        }

    ## Faster workaround

              # temp <- spatial.dat$`20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac`$MASKS$cell_mask$maskraster
              #
              # temp@data
              # temp
              #
              # length(unique(values(temp)))
              #
              #
              # ?extent
              # extent(temp) # X (columns) = 500, Y (rows) = 501
              # length(values(temp))
              #
              # # x <- unique(temp@extent[1]:temp@extent[2])
              # # y <- unique(temp@extent[3]:temp@extent[4])
              # #
              #
              # maxX <- temp@extent[2]
              # maxY <- temp@extent[4]
              #
              # c(0.5:499.5)
              # c(0.5:500.5)
              #
              # x <- c(temp@extent[1]+1:temp@extent[2])
              # y <- c(temp@extent[3]+1:temp@extent[4])
              #
              # z <- matrix(values(temp),
              #             length(y),
              #             length(x),
              #             byrow = TRUE)
              #
              # # https://gis.stackexchange.com/questions/171124/data-frame-to-spatialpolygonsdataframe-with-multiple-polygons
              # dt <- as.data.table(c(1:nrow(z)))
              # names(dt) <- "RowY" # Rows = 501
              # dt
              #
              # dt <- cbind(dt, z)
              # dt
              #
              # names(dt)[length(names(dt))]
              #
              # dt <- gather(dt, 'ColumnX', value = 'Value', names(dt)[2]:names(dt)[length(names(dt))])
              # dt$ColumnX <- gsub("V", "", dt$ColumnX)
              # dt$ColumnX <- as.numeric(dt$ColumnX)
              #
              # names(dt)
              # dt <- dt[,c(2,1,3)] # Reorder columns to X/columns, Y/rows, Values
              # names(dt)
              #
              # setorderv(dt, 'Value')
              # dt
              #
              # max(dt$RowY)
              # max(dt$ColumnX)
              #
              # dt.list <- split(dt, dt$Value)
              # dt.list
              # dt.list <- lapply(dt.list, function(x) { x["Value"] <- NULL; x })
              # dt.list
              #
              # do.list.summary(dt.list)
              #
              # str(dt.list[[969]])
              #
              # ps <- sapply(dt.list, Polygon)
              # p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]),
              #                                                  ID = names(dt.list)[i]  ))
              #
              # my_spatial_polys <- SpatialPolygons(p1)
              # extent(my_spatial_polys)[1] <- 0
              # extent(my_spatial_polys)[3] <- 0
              #
              #
              # my_spatial_polys_df <- SpatialPolygonsDataFrame(my_spatial_polys,
              #                                                 data.frame(id = unique(dt$Value),
              #                                                            row.names = unique(dt$Value)))
              #
              # names(my_spatial_polys_df) <- mask.name <- "cell_mask"
              #
              #
              #
              # my_outline <- fortify(my_spatial_polys_df)
              #
              # #plot(my_spatial_polys)
              # plot(my_spatial_polys_df)

    outline <- fortify(polygon)
    spatial.dat[[i]]$MASKS[[mask.name]][["outlines"]] <- outline
    message("... outlines complete")

    centroids <- gCentroid(polygon,byid=TRUE)
    spatial.dat[[i]]$MASKS[[mask.name]][["centroids"]] <- centroids
    message("... centroids complete")

    #end.time <- Sys.time()
    #time.taken <- end.time - start.time
    #message(paste0("----- ROI ", i, " complete"))
    #message(paste0("----- ROI ", i, " complete in ", time.taken, " minutes"))
  }

  # spatial.dat[[1]]$rasters
  # spatial.dat[[1]]$cell_mask
  # spatial.dat[[1]]$cell_mask_polygons
  # spatial.dat[[1]]$cell_mask_outlines
  # spatial.dat[[1]]$cell_mask_centroids

  message("Returning spatial data")
  return(spatial.dat)
}
