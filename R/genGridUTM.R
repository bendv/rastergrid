#' @title Generate Grid (UTM version)
#' @description Generate grid from raster data in a UTM projection
#' 
#' @param tilesize Numeric. Size (in # pixels) of the square tiles
#' @param extent Extent. Object of type \code{extent} from the target raster
#' @param res Numeric. Resolution of the target raster
#' @param overlap Numeric. Optional: size of overlap region (in # of pixels)
#' @param polygon Logical. Output a SpatialPolygons object as well?
#' @param prj Character. CRS projection string if \code{polygon} is \code{TRUE}
#' 
#' @return \code{data.frame} with extent parameters (xmin, xmax, ymin, ymax) for each tile per row or a \code{list} containing a \code{data.frame} and a \code{SpatialPolygons} object if \code{polygon} is \code{TRUE}
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @import maptools
#' @export
#' 
#' @examples
#' \dontrun{
#' library(bfastSpatial)
#' data(tura)
#' 
#' tiles <- genGridUTM(50, extent(tura), 30)
#' print(tiles)
#' 
#' tiles <- genGridUTM(50, extent(tura), 30, polygon = TRUE, prj = projection(tura))
#' print(tiles)
#' plot(tura, 2)
#' plot(tiles$poly, add = TRUE)
#' 
#' tiles <- genGridUTM(50, extent(tura), 30, overlap = 5, polygon = TRUE, prj = projection(tura))
#' plot(tura, 2)
#' plot(tiles$poly, add = TRUE)
#' }

genGridUTM <-
  function(tilesize, extent, res, overlap=NULL, polygon=FALSE, prj=NULL)
    
  {
    # take the first element of res, if length > 1
    if(length(res) > 1)
      res <- res[1]
    
    # set overlap value (in m)
    if(is.null(overlap))
      overlap <- 0
    
    # divide overlap by 2, since extension will go in both directions
    overlap <- overlap / 2
    
    width <- xmax(extent) - xmin(extent)
    height <- ymax(extent) - ymin(extent)
    
    if(width <= (tilesize * res)) {
      xmins <- xmin(extent)
      xmaxes <- xmax(extent)
    } else {
      xmins <- seq(xmin(extent), xmax(extent), by=tilesize*res)
      xmaxes <- c(xmins[2:length(xmins)], xmax(extent))
    }
    
    if(height <= (tilesize * res)) {
      ymins <- ymin(extent)
      ymaxes <- ymax(extent)
    } else {
      ymins <- seq(ymin(extent), ymax(extent), by=tilesize*res)
      ymaxes <- c(ymins[2:length(ymins)], ymax(extent))
    }
    
    # extend extents by overlap
    xmins <- c(xmins[1], xmins[2:length(xmins)] - overlap*res)
    ymins <- c(ymins[1], ymins[2:length(ymins)] - overlap*res)
    xmaxes <- c(xmaxes[1:length(xmaxes)-1] + overlap*res, xmaxes[length(xmaxes)])
    ymaxes <- c(ymaxes[1:length(ymaxes)-1] + overlap*res, ymaxes[length(ymaxes)])
    
    # permute mins and maxes to get all possible extents    
    tiles <- expand.grid(xmins, ymins)
    colnames(tiles) <- c("xmin", "ymin")
    
    temp <- expand.grid(xmaxes, ymaxes)
    tiles$xmax <- temp[,1]
    tiles$ymax <- temp[,2]
    
    # reorder the columns so that each row matches a raster extent object
    tiles <- subset(tiles, select = c(xmin, xmax, ymin, ymax))
    A <- (tiles$xmax - tiles$xmin) * (tiles$ymax - tiles$ymin)
    
    # delete redundant rows (ie. where xmin==xmax or ymin==ymax)
    tiles <- tiles[which(A > 0), ]
    
    # generate tile names that can be used when writing subsets to file
    # in the form "row-column", starting from the bottom left tile ("1-1")
    ind <- NULL
    for (j in 1:length(ymins)){
      for (i in 1:length(xmins)){
        ind <- c(ind, paste(j, "-", i, sep=""))
      }
    }
    row.names(tiles) <- ind
    
    # optional: output a SpatialPolygons object for visualization of tiles
    if(polygon){
      
      require(maptools)
      
      if(is.null(prj)){
        warning("No projection provided. Defaulting to WGR1984 UTM zone 1")
        prj <- CRS("+proj=utm +zone=1 +ellps=WGS84 +units=m +no_defs")
      }
      
      poly <- as(extent(as.numeric(tiles[1,])), "SpatialPolygons")
      proj4string(poly) <- prj
      poly <- spChFIDs(poly, row.names(tiles)[1])
      
      for(i in 2:nrow(tiles)){
        tmp <- as(extent(as.numeric(tiles[i, ])), "SpatialPolygons")
        proj4string(tmp) <- prj
        tmp <- spChFIDs(tmp, row.names(tiles)[i])
        poly <- spRbind(poly, tmp)
      }
      
      poly <- SpatialPolygonsDataFrame(poly, data = as.data.frame(tiles))
    }
    
    if(polygon)
      tiles <- list(tiles=tiles, poly=poly)
    
    return(tiles)
  }
