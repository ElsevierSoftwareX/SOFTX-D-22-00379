#' Create a list of buffers from a multi-line shapefile.
#'
#' @param coast1 - Either a SpatialLinesDataFrame object or a full path and file
#' name including extension .shp for a multiline shapefile.
#' The shapefile has to be projected, not in lat/long geographical coordinates.
#' The shapefile should have in attribute table at least an unique ID column
#' named ID and a column named buff_dist that has the distance desired for buffer
#' in adequate measurement units from the file projection
#' (usually meters for a UTM projection).
#'
#' @param dir.name - The full path to the directory where a Buffers subdirectory
#' will be created and the individual buffers will be saved. If the dir.name given
#' does not exist it will be created. If dir.name is NA, the buffers will not be saved
#' as polygon shapefiles on the disk.
#'
#' @return A list of projected SpatialPolygonsDataFrame.
#' @export
#'
#' @examples
#' coast1 <- system.file("extdata", "shore_line.shp", package = "iBluff")
#' buff1 <- bBluff(coast1, NA)
bBluff <- function (coast1, dir.name){

  if(inherits(coast1,"SpatialLinesDataFrame")==TRUE) coast <- coast1 else coast <- shape.read(coast1)
  attrib <- coast@data
  crs <- coast@proj4string

  ID <- attrib$ID
  buff_dist<- attrib$buff_dist

  ln.coast <- coast@lines

  coast.ln <- sp::SpatialLines(ln.coast, crs)

  comb1 <-  utils::combn(1:length(coast.ln),1)

  buff1 <- apply(comb1, 2, function(x) raster::buffer(coast.ln[x], buff_dist[x]))

  buff2 <- apply(comb1, 2, function(x) sp::SpatialPolygonsDataFrame(buff1[[x]], data=attrib[x,], match.ID=FALSE))
  if (is.na(dir.name)){
    dir.name <- NA
  } else {

    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Buffers", sep = ""), showWarnings = FALSE)
    dir.buffers <- paste(dir.name, "/","Buffers", sep = "")

    apply(comb1, 2, function(x)
      raster::shapefile(buff2[[x]], file = paste(dir.buffers, "/buff",ID[x],"_",
                                         buff_dist[x], "m.shp", sep="")))
  }
  return(buff2)
}

