#' Clip / extract the DEM in the existing buffers
#'
#' @param dem - The full path and file name including extension (e.g. .tif) of the
#' raster DEM that will be clipped.
#'
#' @param buff - Either a vector (class character) of full path and file names
#' including extension .shp of the polygon shapefiles, or a list of buffers as
#' SpatialPolygonsDataFrame objects.
#'
#' @param dir.name - The full path to the directory where a DEMs subdirectory
#' will be created and the individual rasters will be saved as geotiff.
#' If the dir.name given does not exist it will be created.
#' If dir.name is NA, the rasters will not be saved on the disk.
#'
#' @return A list of rasters.
#' @export
#'
#' @examples
#' coast1 <- system.file("extdata", "shore_line.shp", package = "iBluff")
#' buff1 <- bBluff(coast1, NA)
#' dem1 <- system.file("extdata", "dem_demo.tif", package = "iBluff")
#' dem3 <- clip.rast(dem1, buff1,NA)
#'
clip.rast <- function(dem, buff, dir.name){

  if (inherits(dem,"RasterLayer")==TRUE)  dem1 <- dem else dem1 <- raster::raster(dem)

  comb1 <-  utils::combn(1:length(buff),1)

  if (inherits(buff[[1]][1], "SpatialPolygonsDataFrame")==TRUE){
    buff <- buff
  } else {
    buff <- apply(comb1, 2, function(x) shape.read(buff[x]))
  }

  # if (class(buff[[1]])[1]!="SpatialPolygonsDataFrame"){
  #
  #   buff <- apply(comb1, 2, function(x) shape.read(buff[x]))
  #
  # } else {
  #   if (class(buff[[1]])[1]=="SpatialPolygonsDataFrame") buff <- buff
  # }

  ## crop and mask

  r2 <- apply(comb1, 2, function(x) raster::crop(dem1, raster::extent(buff[[x]])))

  r3 <- apply(comb1, 2, function(x) raster::mask(r2[[x]], buff[[x]]))

  if (is.na(dir.name)){
    dir.name <- NA
  } else {

    dir.name <- gsub("\\\\", "/", dir.name)
    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","DEMs", sep = ""), showWarnings = FALSE)
    dir.dems <- paste(dir.name, "/","DEMs", sep = "")


    apply(comb1, 2, function(x)
      raster::writeRaster(r3[[x]],
                  file = paste(dir.dems, "/dem", buff[[x]]@data$ID,"_",
                               buff[[x]]@data$buff_dist, "m.tif", sep=""),format="GTiff"))
  }
  return(r3)
}
