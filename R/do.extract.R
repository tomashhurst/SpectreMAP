#' do.extract
#'
#' @export

do.extract <- function(dat, # spatial.data object
                       mask, # name of the mask being summarised
                       name = "CellData",
                       fun = "mean" # type of marker summarisation (mean, median etc)
){

  message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

      require('velox')

  ### Demo data

      # dat <- spatial.dat
      # mask <- "cell_mask"
      # name = "CellData"
      # fun = "mean"
      #
      # str(dat, 4)

  ### Loop for each ROI

      rois <- names(spatial.dat)

      for(roi in rois){
        message(paste0("Processing ", roi))
        # roi <- rois[[1]]

        roi.stack <- spatial.dat[[roi]]$RASTERS
        roi.poly <- spatial.dat[[roi]]$MASKS[[mask]]$polygons

        raster.names <- names(roi.stack)
        ply.df <- as.data.frame(roi.poly)
        ply.df

        ply.centroids <- gCentroid(roi.poly,byid=TRUE)
        ply.centroids.df <- as.data.frame(ply.centroids)
        ply.centroids.df # mask number, with X and Y coordinates

        for(i in raster.names){
          message(paste0("Calculating expression data within cell masks for ", i))
          # i <- raster.names[[1]]
          temp.dat <- roi.stack[[i]]

          ## Slower method
          # extracted.dat <- raster::extract(x = temp.dat, y = roi.poly, df = TRUE) # this is the time consuming step
          # extracted.dat.res <- aggregate(. ~ID, data = extracted.dat, FUN = fun)
          # # #colnames(extracted.dat.res)[2] <- i # should we be removing .tiff here? If we do should be the same in the other read.spatial function, to ensure matching consistency
          #
          # ply.centroids.df <- cbind(ply.centroids.df, extracted.dat.res[2]) ## doing this would remove the necessity to calculate centroids within the 'make.spatial.plot' function

          ## FAST method
          ## Faster options
          vx <- velox(temp.dat)
          res <- vx$extract(sp=roi.poly, fun=mean) # 3293 polygons #3294?
          res <- as.data.table(res)
          names(res) <- i
          ply.centroids.df <- cbind(ply.centroids.df, res) ## doing this would remove the necessity to calculate centroids within the 'make.spatial.plot' function
        }

        ply.centroids.df <- as.data.table(ply.centroids.df)
        spatial.dat[[roi]]$DATA[[name]] <- ply.centroids.df
      }

  ### Return new spatial.dat object

      return(spatial.dat)

}
