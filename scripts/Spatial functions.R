

### Packages
    if(!is.element('raster', installed.packages()[,1]))
      install.packages('raster')
    if(!is.element('tiff', installed.packages()[,1]))
      install.packages('tiff')
    if(!is.element('rgeos', installed.packages()[,1]))
      install.packages('rgeos')

    library('raster')
    library('tiff')
    library('rgeos')

########################################################################################################################
########################################################################################################################
########################################################################################################################

## Read in TIFFs

read.spatial.files <- function(roi.loc, # Corresponding directory of ROI folders
                               rois,

                               multi.tiff = FALSE,
                               correct.extent = TRUE,
                               flip.y = TRUE,
                               value.modifier = 65535,
                               ext = ".tiff"){

    ### Checks

        if(multi.tiff == TRUE){
          if(length(grep(".tif", rois)) != length(rois)){
            stop("It appears that your list of ROIs are not TIFF stack files, and might be directories full of single TIFFs (i.e. one TIFF per channel. If this is correct, please use 'multi.tiff = FALSE'")
          }
        }

    ### Setup
        message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")
        ROI.list <- list()
        spatial.dat <- list()
        setwd(roi.loc)


    ### Loop for ROIs -- one TIFF per ROI (i.e. tiff stack)

        if(multi.tiff == TRUE){
          message("Multi.tiff is not currently supported")
        #
        #   setwd(roi.loc)
        #   ROI.list <- list()
        #
        #   for(i in rois){
        #     # i <- rois[[1]]
        #     message(paste0("Reading TIFF file for ", i))
        #
        #     temp <- readTIFF(i, all = TRUE)
        #     str(temp)
        #
        #     temp <- raster(temp[[1]])
        #
        #     raster.stack <- stack(active.roi)
        #
        #
        #
        #
        #
        #     raster.stack <- stack(active.roi)
        #     ROI.list[[i]]$rasters <- raster.stack
        #   }
        }

    ### Loop for ROIs -- one FOLDER per ROI

        if(multi.tiff == FALSE){
          for(i in rois){
            # i <- rois[[1]]
            message(paste0("Reading TIFF files for ", i))

            setwd(roi.loc)
            setwd(i)
            tiffs <- list.files(pattern = ext)

            ## TIFF loop
            active.roi <- list()

            for(a in tiffs){
              # a <- tiffs[[1]]
              active.roi[[a]] <- readTIFF(a)
              active.roi[[a]] <- raster(active.roi[[a]])

              if(correct.extent == TRUE){
                extent(active.roi[[a]]) <- c(0, dim(active.roi[[a]])[2], 0,dim(active.roi[[a]])[1]) # Y axis - X axis
              }

              if(flip.y == TRUE){
                active.roi[[a]] <- flip(active.roi[[a]], 'y')
              }

              values(active.roi[[a]]) <- values(active.roi[[a]])*value.modifier

              for(n in c(1:length(names(active.roi)))){
                names(active.roi)[n] <- gsub(ext, "", names(active.roi)[n])
              }
            }

            raster.stack <- stack(active.roi)
            ROI.list[[i]]$RASTERS <- raster.stack
          }
        }


    ### Return
        message("Constructing of spatial data object complete")
        return(ROI.list)

}




########################################################################################################################
########################################################################################################################
########################################################################################################################

do.add.masks <- function(spatial.dat,
                         mask.loc,
                         masks,
                         mask.label = "cell_mask",

                         correct.extent = TRUE,
                         flip.y = TRUE,
                         value.modifier = 65535,

                         mask.ext = "_mask.tiff"){
  ### Setup
      message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

      spat.names <- names(spatial.dat)
      spat.names

      mask.names <- gsub(mask.ext, "", masks)
      mask.names

      mask.check <- (spat.names == mask.names)
      mask.check

      if(all(mask.check) == FALSE){
        stop('Error -- list of ROIs does not match the list of masks')
      }

  ### Read in mask files

      setwd(mask.loc)

      for(i in spat.names){
        # i <- spat.names[[3]]

        mask.img <- readTIFF(paste0(i, mask.ext))
        mask.img <- raster(mask.img)

        if(correct.extent == TRUE){
          extent(mask.img) <- c(0, dim(mask.img)[2], 0,dim(mask.img)[1]) # Y axis - X axis
        }

        if(flip.y == TRUE){
          mask.img <- flip(mask.img, 'y')
        }

        values(mask.img) <- values(mask.img)*value.modifier
        names(mask.img) <- mask.label

        spatial.dat[[i]]$MASKS[[mask.label]]$maskraster <- mask.img

      }

      message("Returning spatial data object with added masks")
      return(spatial.dat)
}


########################################################################################################################
########################################################################################################################
########################################################################################################################


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



########################################################################################################################
########################################################################################################################
########################################################################################################################


do.add.percell <- function(spatial.dat,
                           percell.dat,
                           roi.col,
                           name = "per.cell"){

  ### Setup
      message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

  ### Loop

      for(i in names(spatial.dat)){
        # i <- "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac"
        temp <- percell.dat[percell.dat[[roi.col]] == i,]
        spatial.dat[[i]]$CPDATA[[name]] <- temp
      }

  return(spatial.dat)

}


########################################################################################################################
########################################################################################################################
########################################################################################################################

do.filter.percell <- function(spatial.dat,
                              per.cell,
                              to,
                              filter.by,
                              id.col = "ObjectNumber",
                              x.col = "Location_Center_X",
                              y.col = "Location_Center_Y",
                              simplify.cp.colname = TRUE,
                              value.modifier = 65535){

  ### Setup
      message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

  ### Loop

      for(i in names(spatial.dat)){
        # i <- "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac"
          temp <- spatial.dat[[i]]$CPDATA[[per.cell]]
          temp.filtered <- temp[,grepl( filter.by , names(temp)),with = FALSE]
          temp.filtered <- temp.filtered * value.modifier
          temp.filtered <- cbind(ID = temp[[id.col]],x = temp[[x.col]], y = temp[[y.col]], temp.filtered)

          if(simplify.cp.colname == TRUE){
            measr <- temp.filtered[,c(1:3),with = FALSE]
            chnls <- temp.filtered[,c(4:length(names(temp.filtered))),with = FALSE]

            names(chnls) <- sub('.*\\_c', '', names(chnls))
            temp.names <- names(chnls)

            for(b in c(1:length(temp.names))){
              a <- temp.names[[b]]
              if(nchar(a) == 1){
                a <- paste0("0", a)
              }
              temp.names[[b]] <- a
            }

            names(chnls) <- temp.names
            neworder <- sort(names(chnls))
            chnls <- setcolorder(chnls, neworder)

            temp.filtered <- cbind(measr, chnls)
            }

          spatial.dat[[i]]$CPDATA[[to]] <- temp.filtered
        }

  ### Return
      return(spatial.dat)

}




########################################################################################################################
########################################################################################################################
########################################################################################################################

do.extract.cell.dat <- function(spatial.dat,
                                target.dat){

  ### Setup
      message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

  ### Extract

      dat.list <- list()

      for(i in names(spatial.dat)){
        roi.dat <- spatial.dat[[i]]$CPDATA[[target.dat]]
        nme.vec <- rep(i, nrow(roi.dat))

        roi.dat <- cbind("ROI" = nme.vec, roi.dat)
        dat.list[[i]] <- roi.dat
      }

      dat.dt <- rbindlist(dat.list, fill = TRUE)
      return(dat.dt)
}







########################################################################################################################
########################################################################################################################
########################################################################################################################

make.spatial.plot <- function(spatial.dat, # spatial data object
                              image.roi, # name of ROI
                              image.channel, # name of channel

                              ## Options for adding cell outlines
                              mask.outlines = NULL, # character -- the outlines in spatial.dat object

                              ## Options for adding cellular data
                              cell.dat = NULL, # can be character (if it's data within spatial.dat) or a data.table
                              cell.col = NULL, # column for colouration

                              ## Other settings (with defaults)
                              image.y.flip = TRUE,
                              image.mask.size = 0.1,
                              image.mask.colour = "gold",
                              image.min.threshold = 0.00,
                              image.max.threshold = 0.99,
                              image.blank = FALSE,

                              cell.x = "x",
                              cell.y = "y",
                              cell.col.type = "numeric",
                              cell.colours = "spectral",
                              cell.col.min.threshold = 0.01,
                              cell.col.max.threshold = 0.995,

                              title = paste0(image.roi),
                              dot.size = 1,
                              dot.alpha = 1,
                              align.xy.by = cell.dat, # choose a data frame to set absolute limits for X/Y/colour
                              align.col.by = cell.dat,
                              save.to.disk = TRUE,
                              path = getwd(),
                              plot.width = 9,
                              plot.height = 7,
                              blank.axis = FALSE)
{

  ### TESTING
  # library(raster)
  # library(data.table)
  # library(tiff)
  # library(ggplot2)
  #
  # spatial.dat = spatial.dat
  #
  # spatial.dat$meta.data
  #
  # roi = "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac"
  # roi.marker = "CD20_Dy161"

  # cell.dat <- spatial.dat$cell.dat.means.filtered
  # cell.dat <- cell.dat[cell.dat[["ImageName"]] == "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac_ilastik_s2_Probabilities_mask.tiff",]
  # cell.dat = cell.dat
  # cell.x = "X"
  # cell.y = "Y"
  # cell.colour = 'CD20'
  #
  # add.outlines = TRUE
  # flip.y.axis = TRUE

  message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")

  ### Check that necessary packages are installed
      if(!is.element('Spectre', installed.packages()[,1])) stop('Spectre is required but not installed')
      if(!is.element('ggplot2', installed.packages()[,1])) stop('ggplot2 is required but not installed')
      if(!is.element('scales', installed.packages()[,1])) stop('scales is required but not installed')
      if(!is.element('colorRamps', installed.packages()[,1])) stop('colorRamps is required but not installed')
      if(!is.element('ggthemes', installed.packages()[,1])) stop('ggthemes is required but not installed')
      if(!is.element('RColorBrewer', installed.packages()[,1])) stop('RColorBrewer is required but not installed')
      if(!is.element('raster', installed.packages()[,1])) stop('raster is required but not installed')
      if(!is.element('rgeos', installed.packages()[,1])) stop('rgeos is required but not installed')

  ### Require packages
      require(Spectre)
      require(ggplot2)
      require(scales)
      require(colorRamps)
      require(ggthemes)
      require(RColorBrewer)
      require(raster)
      require(rgeos)

  ### Compatability conversions

      roi <- image.roi
      roi.marker <- image.channel

      #cell.dat
      cell.colour <- cell.col

      add.outlines <- image.outlines <- mask.outlines
      flip.y.axis <- image.y.flip

      cell.colour.type <- cell.col.type

      raster.mask.size <- image.mask.size
      raster.mask.colour <- image.mask.colour
      raster.min.threshold <- image.min.threshold
      raster.max.threshold <- image.max.threshold

      col.min.threshold <- cell.col.min.threshold
      col.max.threshold <- cell.col.max.threshold

      colours <- cell.colours

  ### Colour setup

      # Jet
      if(colours == "jet"){
        colour.scheme <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
      }

      # Spectral
      if(colours == "spectral"){
        spectral.list <- colorRampPalette(brewer.pal(11,"Spectral"))(50)
        spectral.list <- rev(spectral.list)
        colour.scheme <- colorRampPalette(c(spectral.list))
      }

      # Viridis
      if(colours == "viridis"){
        colour.scheme <- colorRampPalette(c(viridis_pal(option = "viridis")(50)))
      }

      # Inferno
      if(colours == "inferno"){
        colour.scheme <- colorRampPalette(c(viridis_pal(option = "inferno")(50)))
      }

      #Magma
      if(colours == "magma"){
        colour.scheme <- colorRampPalette(c(viridis_pal(option = "magma")(50)))
      }

  ### cell.dat setup

      if(!is.null(cell.dat)){

        if(is.character(cell.dat) == TRUE){
          temp <- spatial.dat[[roi]]$CPDATA[[cell.dat]]
          cell.dat <- temp
        }


        if(cell.colour.type == "numeric"){

          # Dot point colouration
          if(is.null(align.col.by) == TRUE){
            ColrMin <- quantile(cell.dat[[cell.colour]], probs = c(col.min.threshold))
            ColrMax <- quantile(cell.dat[[cell.colour]], probs = c(col.max.threshold))
          }

          if(is.null(align.col.by) == FALSE){
            ColrMin <- quantile(align.col.by[[cell.colour]], probs = c(col.min.threshold))
            ColrMax <- quantile(align.col.by[[cell.colour]], probs = c(col.max.threshold))
          }

        }

      }


  ### Preparat the raster data

      ## Image prep

      raster.image <- spatial.dat[[roi]]$RASTERS[[roi.marker]]

      tiff.p <- rasterToPoints(raster.image)
      tiff.df <- data.frame(tiff.p)
      raster.label <- names(tiff.df)[3]
      colnames(tiff.df) <- c("x_axis", "y_axis", raster.label)

      ## Create cell outlines
      if(!is.null(mask.outlines)){
        outline <- spatial.dat[[roi]]$MASKS[[mask.outlines]]$outlines
        centroids <- spatial.dat[[roi]]$MASKS[[mask.outlines]]$centroids

        centroid.xmin <- centroids@bbox[1]
        centroid.xmax <- centroids@bbox[3]

        centroid.ymin <- centroids@bbox[2]
        centroid.ymax <- centroids@bbox[4]
      }

      ## Flip y-axis values

      # if(flip.y.axis == TRUE){
      #   dat <- invert.y.axis(dat, y.axis)
      # }

      ## Normalise XY for cell centroids
      plot.normalize <- function(dat, min, max){
        return(((dat- min(dat)) / (max(dat)-min(dat))) * (max - min) + min)
      }

      # if(!is.null(cell.dat)){
      #   # X AXIS
      #   cell.dat[[cell.x]] <- plot.normalize(cell.dat[[cell.x]], min = centroid.xmin, max = centroid.xmax)
      #
      #   # Y AXIS
      #   cell.dat[[cell.y]] <- plot.normalize(cell.dat[[cell.y]], min = centroid.ymin, max = centroid.ymax)
      #
      # }

      ## Raster colour limits

      RastMin <- quantile(tiff.df[[3]], probs = c(raster.min.threshold))
      RastMax <- quantile(tiff.df[[3]], probs = c(raster.max.threshold))

  ###############################################
  ### Add a check to see if centroids line up ###
  ###############################################

  ### Generate and show coloured plot

      if(image.blank == FALSE){
        p <- ggplot(data=tiff.df, aes(x=tiff.df[[1]], y=tiff.df[[2]])) +

          ## Plot the raster (IMC image)
          geom_raster(aes(fill=tiff.df[[3]])) +
          scale_fill_gradient(raster.label,
                              low = "black",
                              high = "white",
                              limits=c(RastMin,RastMax),
                              oob=squish)
      }

      if(image.blank == TRUE){
        p <- ggplot(data=tiff.df, aes(x=tiff.df[[1]], y=tiff.df[[2]])) +

          ## Plot the raster (IMC image)
          geom_raster(aes(fill=tiff.df[[3]])) +
          scale_fill_gradient(raster.label,
                              low = "black",
                              high = "black",
                              limits=c(RastMin,RastMax),
                              oob=squish)
      }



  ### Plot the cell mask boundaries

      if(!is.null(image.outlines)){
        p <- p + geom_path(aes(x = long, y = lat, group = group),
                           data = outline,
                           size = raster.mask.size,
                           col = raster.mask.colour)
      }

  ## Plot the cellular data

      if(!is.null(cell.dat)){
        if(cell.colour.type == "numeric"){
          p <- p + geom_point(data=cell.dat,
                              aes(x=cell.dat[[cell.x]], y=cell.dat[[cell.y]], color = cell.dat[[cell.colour]]),  #as.numeric(as.character(col))
                              size = dot.size, #dot.size
                              alpha = dot.alpha # shape = 1
                              ) +

            scale_color_gradientn(colours = colour.scheme(50),
                                  limits = (c(ColrMin,ColrMax)),
                                  oob=squish,
                                  name = cell.colour)
        }

        if(cell.colour.type != "numeric"){
          p <- p + geom_point(data=cell.dat,
                             aes(x=cell.dat[[cell.x]], y=cell.dat[[cell.y]], color = as.factor(cell.dat[[cell.colour]])),  #as.numeric(as.character(col))
                             size = dot.size, #dot.size
                             alpha = dot.alpha # shape = 1
                             ) +

            scale_colour_discrete(name = cell.colour)
        }

      }

      ## Setup some themes
      p <- p + theme_bw() +
        coord_equal() +
        xlab(cell.x)+
        ylab(cell.y)+
        ggtitle(title)

      ## More themes
      p <- p + theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # change 'colour' to black for informative axis
                     axis.title.x=element_text(color="Black", face="bold", size=18),
                     axis.title.y=element_text(color="Black", face="bold", size=18),
                     legend.text=element_text(size=12), # large = 30 # small = 8
                     legend.key.height=unit(1,"cm"), # large = 3 # small = 1.2
                     legend.key.width=unit(0.4,"cm"), # large = 1 # small = 0.4
                     #legend.title=element_blank(),
                     plot.title = element_text(color="Black", face="bold", size=16, hjust=0) # size 70 for large, # 18 for small
      )

      if(flip.y.axis == TRUE){

        p <- p + scale_y_reverse()

      }

      ### Save ggplot to disk if desired
      if(save.to.disk == TRUE){
        ggsave(filename = paste0(title, "_ROI_", roi.marker, "_marker_", cell.colour,".png"),
               plot = p,
               path = path,
               width = plot.width,
               height = plot.height,
               limitsize = FALSE)
      }

      print(p)

}







########################################################################################################################
########################################################################################################################
########################################################################################################################

do.label.from.polygon <- function(spatial.dat,
                                  cell.dat,
                                  mask,
                                  labels,

                                  name = "Label",
                                  id.col = 'ID',
                                  roi.col = 'ROI'#,
                                  #x.col = 'x',
                                  #y.col = 'y'
){

  ###

  # spatial.dat <- spatial.dat
  # cell.dat <- cell.dat
  # mask <- 'obj_mask'
  #
  # name = "Label"
  # labels <- c("HTC", "CTL", "Background", "Hepatocyte", "cDC", "CD11bpos", "Other immune", "Macrophages")
  #
  # id.col = 'ID'
  # roi.col = 'ROI'
  # x.col = 'x'
  # y.col = 'y'

  TEMP_LABEL_PLACEHOLDER1 <- labels
  TEMP_LABEL_PLACEHOLDER1 <- as.data.table(TEMP_LABEL_PLACEHOLDER1)

  ###
  rois <- names(spatial.dat)

  start.dat.list <- list()
  dat.list <- list()
  mask.list <- list()

  unq.labels <- list()
  res.list <- list()

  ###

  for(i in rois){
    unq.labels[[i]] <- unique(as.data.frame(spatial.dat[[i]]$MASKS[[mask]]$polygons))
    rm(i)
  }

  all.labs <- unique(rbindlist(unq.labels, fill = TRUE))
  all.labs <- all.labs[order(all.labs)]

  all.labs <- cbind(all.labs, TEMP_LABEL_PLACEHOLDER1)
  all.labs

  for(i in rois){
    # i <- rois[[8]]
    dat.list[[i]] <- cell.dat[cell.dat[[roi.col]] == i,]
    coordinates(dat.list[[i]]) <- ~ x + y

    mask.list[[i]] <- spatial.dat[[i]]$MASKS[[mask]]$polygons
    proj4string(dat.list[[i]]) <- proj4string(mask.list[[i]])

    res <- over(dat.list[[i]], mask.list[[i]])
    res.list[[i]] <- res[[1]]
  }

  ###

  res.dt <- unlist(res.list)
  res.dt <- as.data.table(res.dt)
  res.dt

  if(nrow(cell.dat) != nrow(res.dt)){
    stop("Result dt rows are inconsistent with starting dt rows")
  }

  res.dt <- do.embed.columns(res.dt, "res.dt", all.labs, 'obj_mask')
  res.dt

  res.dt$res.dt <- NULL
  names(res.dt)[length(names(res.dt))] <- name
  res.dt

  return.dat <- cbind(cell.dat, res.dt)

  ###
  return(return.dat)
}




