###################################################################################
### Setup
###################################################################################

    ### Install SpectreMAP

        library('devtools')
        install_github("tomashhurst/SpectreMAP")

    ### Load packages

        library(Spectre)
        package.check()
        package.load()


        library(SpectreMAP)
        install_github("hunzikp/velox")
        library('velox')

        library(raster)

    ### Set directories

        dirname(rstudioapi::getActiveDocumentContext()$path)            # Finds the directory where this script is located
        setwd(dirname(rstudioapi::getActiveDocumentContext()$path))     # Sets the working directory to where the script is located
        getwd()
        start.dir <- getwd()
        start.dir

        dir.create("SpectreMAP_output")
        setwd("SpectreMAP_output")
        output.dir <- getwd()

###################################################################################
### Read in images and masks
###################################################################################

    ### Read TIFF files into spatial.dat object

        setwd(start.dir)
        setwd("ROIs/")

        rois <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
        rois

        spatial.dat <- read.spatial.files(roi.loc = getwd(), rois = rois)

    ### Check names of channels

        as.matrix(names(spatial.dat)) # ROI names
        names(spatial.dat[[1]]) # only rasters  currently in the data
        as.matrix(names(spatial.dat[[1]]$RASTERS)) # TIFF names of first ROI

    ### Read in masks

        setwd(start.dir)
        setwd("Masks")

        list.files()

        mask.ext <- "_ilastik_s2_Probabilities_mask.tiff"

        masks <- list.files(pattern = mask.ext)
        masks

        spatial.dat <- do.add.masks(spatial.dat = spatial.dat,
                                    mask.loc = getwd(),
                                    masks = masks,
                                    mask.ext = mask.ext,
                                    mask.label = "cell_mask")

        names(spatial.dat[[1]])

        spatial.dat[[1]]$RASTERS
        spatial.dat[[1]]$MASKS

        raster::rasterToPoints(spatial.dat[[1]]$RASTERS$X127I_I127)

        library(sf)
        library(stars)


        raster::raster(spatial.dat[[1]]$RASTERS$X127I_I127)

        # x <- st_as_stars(spatial.dat[[1]]$RASTERS$X127I_I127) %>%
        #   st_as_sf(merge = TRUE) %>% # this is the raster to polygons part
        #   st_cast("MULTILINESTRING") # cast the polygons to polylines
        #



    ### Create cell outlines

        spatial.dat <- do.create.outlines(spatial.dat = spatial.dat, mask.name = "cell_mask")

        as.matrix(names(spatial.dat[[1]]))
        as.matrix(names(spatial.dat[[1]]$MASKS$cell_mask))

        str(spatial.dat, 3)

        spatial.dat[[1]]$MASKS$cell_mask$polygons
        spatial.dat[[1]]$MASKS$cell_mask$outlines

        plot(spatial.dat[[1]]$MASKS$cell_mask$polygons)

    ### Summarise 'per cell' data

        spatial.dat <- do.extract(dat = spatial.dat, mask = "cell_mask", name = "CellData", fun = "mean")


###################################################################################
### Make some spatial plots
###################################################################################

    setwd(output.dir)
    dir.create("Plots")
    setwd("Plots")

    make.spatial.plot(spatial.dat = spatial.dat,
                      image.roi = '20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac',
                      image.channel = "CD20_Dy161",
                      mask.outlines = "cell_mask",
                      cell.dat = "CellData",
                      cell.col = "CD20_Dy161")


###################################################################################
### Create 'cellular' data and plot
###################################################################################

    setwd(output.dir)
    dir.create("Plots")
    setwd("Plots")

    ### Extract 'cellular' data from each ROI and combine into a single data.table

        cell.dat <- do.extract.cell.dat(spatial.dat, 'CellData')
        cell.dat

        as.matrix(names(cell.dat))

        cell.dat <- do.asinh(cell.dat, names(cell.dat)[c(3:27)], cofactor = 1)
        cell.dat

        make.colour.plot(cell.dat, "CD20_Dy161_asinh", "CD3_Er170_asinh")

        make.spatial.plot(spatial.dat = spatial.dat,
                          image.roi = '20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac',
                          image.channel = "CD20_Dy161",
                          mask.outlines = "cell_mask",
                          cell.dat = cell.dat[cell.dat[['ROI']] == '20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac',],
                          cell.col = "CD20_Dy161_asinh")


###################################################################################
### Save spatial.data file
###################################################################################

    ### Set working dir

        setwd(output.dir)

        saveRDS(spatial.dat, 'spatial.dat.rds')
        fwrite(cell.dat, "cell.dat.csv")

