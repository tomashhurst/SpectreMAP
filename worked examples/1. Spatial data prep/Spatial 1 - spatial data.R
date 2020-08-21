
###################################################################################
### Read in images and masks
###################################################################################

### Load packages

    library(Spectre)
    package.check()
    package.load()

    # package.check(type = "spatial")
    # package.load(type = "spatial")

### Set directories

    dirname(rstudioapi::getActiveDocumentContext()$path)            # Finds the directory where this script is located
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))     # Sets the working directory to where the script is located
    getwd()
    start.dir <- getwd()
    start.dir

### Read TIFF files into spatial.dat object

    setwd("ROIs/")
    roi.dir <- getwd()

    rois <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
    rois

    spatial.dat <- read.spatial.files(roi.loc = roi.dir,
                                       rois = rois)

### Check names of channels

    as.matrix(names(spatial.dat)) # ROI names
    names(spatial.dat[[1]]) # only rasters  currently in the data
    as.matrix(names(spatial.dat[[1]]$rasters)) # TIFF names of first ROI

### Read in masks

    setwd(start.dir)
    setwd("CPoutput/")
    mask.dir <- getwd()

    list.files()
    mask.ext <- "_ilastik_s2_Probabilities_mask.tiff"
    masks <- list.files(pattern = mask.ext)
    masks

    spatial.dat <- do.add.masks(spatial.dat = spatial.dat,
                                mask.loc = mask.dir,
                                masks = masks,
                                mask.ext = mask.ext,
                                mask.label = "cell_mask")

    names(spatial.dat[[1]])

    spatial.dat[[1]]$rasters
    spatial.dat[[1]]$cell_mask


### Create cell outlines

    spatial.dat <- do.create.outlines(spatial.dat = spatial.dat,
                                      mask.name = "cell_mask")

    as.matrix(names(spatial.dat[[1]]))
    as.matrix(names(spatial.dat[[1]]$cell_mask))


###################################################################################
### Read in 'per cell' data
###################################################################################

### Read in 'per cell' data

    setwd(start.dir)
    setwd("CPoutput/")

    list.files(getwd(), ".csv")

    percell.dat <- fread("cell.csv")
    image.dat <- fread("Image.csv")


    unique(percell.dat$ImageNumber)
    add <- image.dat[,c('ImageNumber', 'FileName_CellImage'), with= FALSE]

    percell.dat <- do.embed.columns(dat = percell.dat,
                                    base.col = "ImageNumber",
                                    add.dat = add,
                                    add.by = 'ImageNumber')

    percell.dat$FileName_CellImage

    temp <- gsub("_ilastik_s2_Probabilities_mask.tiff", "", percell.dat$FileName_CellImage)
    percell.dat$ImageName <- temp

    as.matrix(unique(percell.dat$ImageName)) # Image names

    percell.dat$Location_Center_X # X coordinates for per cell data
    percell.dat$Location_Center_Y # Y coordinates for per cell data


### Add to spatial.dat

    percell.dat$ImageName

    spatial.dat <- do.add.percell(spatial.dat = spatial.dat,
                                  percell.dat = percell.dat,
                                  roi.col = "ImageName",
                                  name = "per.cell")

### Filter and adjust the most useful datasets

    filters <- c("Intensity_MeanIntensity_FullStack_",
                 "Intensity_MedianIntensity_FullStack_",
                 "Intensity_IntegratedIntensity_FullStack_",
                 "Intensity_MeanIntensity_FullStackFiltered_",
                 "Intensity_MedianIntensity_FullStackFiltered_",
                 "Intensity_IntegratedIntensity_FullStackFiltered_")

    nms <- gsub("_FullStack", "", filters)

    for(i in c(1:length(filters))){
      # i <- 1

      filter <- filters[[i]]
      nm <- nms[[i]]

      spatial.dat <- do.filter.percell(spatial.dat = spatial.dat,
                                       per.cell = "per.cell",
                                       to = nm,
                                       filter.by = filter,
                                       id.col = "ObjectNumber",
                                       x.col = "Location_Center_X", # uses x
                                       y.col = "Location_Center_Y", # uses y
                                       simplify.cp.colname = TRUE,
                                       value.modifier = 65535)
      }


    as.matrix(names(spatial.dat[[1]]))
    spatial.dat[[1]]$Intensity_MeanIntensity_
    spatial.dat[[1]]$Intensity_IntegratedIntensityFiltered_


###################################################################################
### Rename measured columns using the 'panel'
###################################################################################

    ### Rename 'columns

        setwd(start.dir)
        setwd("panel/")

        panel <- fread("panel.csv")
        panel

        ##
        new.names <- panel$Target
        as.matrix(new.names)

        ##
        as.matrix(names(spatial.dat[[1]]$Intensity_MeanIntensity_))

        target.nums <- c(4:16)
        target.names <- names(spatial.dat[[1]]$Intensity_MeanIntensity_)[target.nums]
        as.matrix(target.names)

        for(i in names(spatial.dat)){
          # i <- names(spatial.dat)[[1]]
          for(a in nms){
            # a <- nms[[1]]
              names(spatial.dat[[i]][[a]])[target.nums] <- new.names
          }
        }

        as.matrix(names(spatial.dat[[1]]$Intensity_MeanIntensity_))
        as.matrix(names(spatial.dat[[1]]$Intensity_IntegratedIntensityFiltered_))

        as.matrix(names(spatial.dat[[2]]$Intensity_MeanIntensity_))
        as.matrix(names(spatial.dat[[2]]$Intensity_IntegratedIntensityFiltered_))

### Quick visualisation test

    as.matrix(names(spatial.dat))
    as.matrix(names(spatial.dat$`20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac`$rasters))
    as.matrix(names(spatial.dat$`20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac`$Intensity_MeanIntensityFiltered_))

    make.spatial.plot(spatial.dat,
                      image.roi = "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac",
                      image.channel = "CD20_Dy161",
                      mask.outlines = "cell_mask",
                      cell.dat = "Intensity_MeanIntensity_",
                      cell.col = "CD20")

### Save spatial data object to disk

    setwd(start.dir)
    saveRDS(spatial.dat, file = "spatial.dat.rds")


    save(spatial.dat, file = "spatial.dat.RDATA")
