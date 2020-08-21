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
  message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")
  
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
    
    ## Fast version
    # if(use.gdal == TRUE){
    #   if(length(Sys.which("gdal_polygonize.py")) > 1){
    #     polygon <- gdal_polygonizeR(x = mask,  pypath = pypath)
    #   } else {
    #   }
    # }
    
    ## Older andslower version
    if(use.gdal == FALSE){
      polygon <- rasterToPolygons(mask, dissolve=TRUE) # This is the long step
      spatial.dat[[i]]$MASKS[[mask.name]][["polygons"]] <- polygon
    }
    
    ## Python enhanced version
    
    
    ##
    
    # library(parallel)
    # cl <- makeCluster(detectCores())
    # df <- parSapply(cl, args, myFunction)
    
    ##
    #install.packages("remotes")
    #remotes::install_github("lbusett/sprawl")
    # library('sprawl')
    #
    # install.packages("velox")
    # library('velox')
    
    #polygon <- gdal_polygonizeR(mask)
    
    ##
    # r.to.poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(mask),
    #                                          as_points = FALSE,
    #                                          merge = TRUE))
    
    ##
    #getValues(mask)
    
    ## Faster version
    #install.packages('stars')
    # require('stars')
    # x <- st_as_stars(mask) %>%
    #   st_as_sf(merge = TRUE) %>% # this is the raster to polygons part
    #   st_cast("MULTILINESTRING") # cast the polygons to polylines
    #
    # plot(x)
    
    ## Another fast version
    #( e <- extract(r, p) )
    
    outline <- fortify(polygon)
    spatial.dat[[i]]$MASKS[[mask.name]][["outlines"]] <- outline
    
    centroids <- gCentroid(polygon,byid=TRUE)
    spatial.dat[[i]]$MASKS[[mask.name]][["centroids"]] <- centroids
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    message(paste0("----- ROI ", i, " complete in ", time.taken, " minutes"))
  }
  
  # spatial.dat[[1]]$rasters
  # spatial.dat[[1]]$cell_mask
  # spatial.dat[[1]]$cell_mask_polygons
  # spatial.dat[[1]]$cell_mask_outlines
  # spatial.dat[[1]]$cell_mask_centroids
  
  message("Returning spatial data")
  return(spatial.dat)
}