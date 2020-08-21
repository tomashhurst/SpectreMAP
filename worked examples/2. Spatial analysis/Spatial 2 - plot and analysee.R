
### Load packages

library(Spectre)
package.check()
package.load()

package.check(type = "spatial")
package.load(type = "spatial")

### Set directories

dirname(rstudioapi::getActiveDocumentContext()$path)            # Finds the directory where this script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))     # Sets the working directory to where the script is located
getwd()
start.dir <- getwd()
start.dir

### Read in spatial data file

spatial.dat <- readRDS("spatial.dat.rds")

dir.create("Spectre-spatial-output")
setwd("Spectre-spatial-output")

output.dir <- getwd()



as.matrix(names(spatial.dat))
as.matrix(names(spatial.dat[[1]]))
as.matrix(names(spatial.dat[[1]]$rasters))
as.matrix(names(spatial.dat[[1]]$cell_mask))
as.matrix(names(spatial.dat[[1]][[4]]))

### Some spatial plots

setwd(output.dir)
dir.create("Spatial plots")
setwd("Spatial plots")

as.matrix(names(spatial.dat))
as.matrix(names(spatial.dat[[1]]))

make.spatial.plot(spatial.dat,
                  image.roi = "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac",
                  image.channel = "CD20_Dy161",
                  mask.outlines = "cell_mask",
                  title = "Image + mask outline")

make.spatial.plot(spatial.dat,
                  image.roi = "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac",
                  image.channel = "CD20_Dy161",
                  mask.outlines = "cell_mask",
                  cell.dat = "Intensity_MeanIntensity_",
                  cell.col = "CD20",
                  dot.size = 0.8,
                  title = "Image + mask outline + internal cell data")

for(i in names(spatial.dat)){
  make.spatial.plot(spatial.dat,
                    image.roi = i,
                    image.channel = "CD20_Dy161"#,
                    #mask.outlines = "cell_mask",
                    #cell.dat = "Intensity_MeanIntensity_",
                    #cell.col = "CD20"
  )
}



### Extract cellular data for analysis

setwd(output.dir)
dir.create("Cellular data plots")
setwd("Cellular data plots")

cell.dat <- do.extract.cell.dat(spatial.dat = spatial.dat,
                                target.dat = "Intensity_MeanIntensity_")

cell.dat

### asinh transformation
as.matrix(names(cell.dat))

to.transfer <- names(cell.dat)[c(5:17)]
to.transfer

cell.dat <- do.asinh(dat = cell.dat,
                     use.cols = to.transfer,
                     cofactor = 2,
                     reduce.noise = FALSE)
cell.dat

to.transfer <- paste0(to.transfer, "_asinh_cf2")

## PLOTS
setwd(output.dir)
setwd("Cellular data plots")
dir.create("Axis checks")
setwd("Axis checks")

for(i in to.transfer){
  make.density.plot(cell.dat, i, "CD20_asinh_cf2")
}

setwd(output.dir)
setwd("Cellular data plots")

### normalise (with zero drop - experimental)

as.matrix(to.transfer)

# to.zero <- c("CD3_asinh_cf2",
#              "CD20_asinh_cf2",
#              "CD11b_asinh_cf2",
#              "CD45_asinh_cf2",
#              "CD68_asinh_cf2",
#              "Collagen_I_asinh_cf2")
#
# zeros <- c(0.4,
#            1,
#            0.4,
#            0.8,
#            2,
#            0.6)
#
# no.zero <- to.transfer[c(1,2,5,8,11:13)]
# no.zero
#
# ##
#
# cell.dat <- do.normalise(cell.dat,
#                          use.cols = no.zero)
#
# cell.dat <- do.normalise(cell.dat,
#                          use.cols = to.zero,
#                          zero.drops = zeros)

cell.dat <- do.normalise(cell.dat,
                         use.cols = to.transfer)

to.transfer <- paste0(to.transfer, "_norm")


cell.dat

setwd(output.dir)
setwd("Cellular data plots")
dir.create("Normalised axis checks")
setwd("Normalised axis checks")

as.matrix(names(cell.dat))

for(i in to.transfer){
  make.density.plot(cell.dat, i, "CD20_asinh_cf2_norm")
}

setwd(output.dir)
setwd("Cellular data plots")



### Run clustering and DR

as.matrix(names(cell.dat))

markers <- names(cell.dat)[c(33:34,36:38,40)]
markers

cell.dat <- run.flowsom(cell.dat, markers, meta.k = 10)
cell.dat <- run.tsne(cell.dat, markers, perplexity = 50)

#cell.dat <- cell.dat[cell.dat[["UMAP_X"]] < 10,]

setwd(output.dir)
setwd("Cellular data plots")

make.density.plot(cell.dat, "tSNE_X", "tSNE_Y")

make.colour.plot(cell.dat, "tSNE_X", "tSNE_Y", "FlowSOM_metacluster", col.type = "factor", add.label = TRUE)
make.colour.plot(cell.dat, "tSNE_X", "tSNE_Y", "ROI", plot.width = 11, col.type = "factor")

make.multi.plot(cell.dat, "tSNE_X", "tSNE_Y",
                plot.by = c("FlowSOM_metacluster", "ROI"),
                figure.title = "Clusters and ROIs",
                #col.min.threshold = 0,
                col.max.threshold = 0.995,
                col.type = "factor",
                add.density = TRUE)

make.multi.plot(cell.dat, "tSNE_X", "tSNE_Y",
                plot.by = c(markers),
                figure.title = "Markers",
                #col.min.threshold = 0,
                col.max.threshold = 0.995, add.density = TRUE)




#
# make.spatial.plot(spatial.dat,
#                   image.roi = "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac",
#                   image.channel = "CD20_Dy161",
#                   mask.outlines = "cell_mask",
#                   cell.dat = cell.dat[cell.dat[["ROI"]] == "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac",],
#                   cell.col = "FlowSOM_metacluster",
#                   cell.col.type = "factor",
#                   title = "External data")
