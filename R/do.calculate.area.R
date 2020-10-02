#' do.calculate.area
#'
#' @import data.table
#'
#' @export

do.calculate.area <- function(dat,
                              region){
  
  ### Setup
  
  # dat <- spatial.dat
  # region <- 'regions'
  
  ### Preparation
  
  roi.names <- names(dat)
  
  ### Processing
  
  area.list <- list()
  
  for(i in roi.names){
    # i <- 8
    
    poly.names <- dat[[i]]$MASKS[[region]]$polygons@data
    areas <- area(dat[[i]]$MASKS[[region]]$polygons)
    
    areas <- as.data.table(t(areas))
    names(areas) <- as.character(poly.names[[1]])
    area.list[[i]] <- areas
  }
  
  area.list
  
  area.res <- rbindlist(area.list, fill = TRUE)
  area.res <- cbind(roi.names, area.res)
  names(area.res)[1] <- "ROI"
  
  ### Return
  
  return(area.res)
  
}