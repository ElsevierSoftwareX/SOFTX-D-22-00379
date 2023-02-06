#' Save a point, line, and smoothed line shapefile
#'
#' The function transform a table of x and y coordinates in a point and line shapefile
#' with projection if a raster layer or a full path to a raster is given to read
#' the desired projection from., otherwise the projection string will be empty.
#'
#' @param tabl - Is a table with the first 2 columns x and y coordinates or a list
#' of tables with the first 2 columns x and y coordinates/
#'
#' @param dir.shape - Full path of the directory where to save the shapefiles.
#'
#' @param file.name - A name used as prefix for all saved shapefiles.
#'
#' @param smooth.win - How big the smoothing window is, represented by  the number
#' of cells to smooth over. If smooth.win=NA (default) no smoothing will take place.
#'
#' @param rast1 - Either full path including file name and extension (e.g. .tif for geotiff files)
#' of one DEM raster that was clipped with a buffer or a RasterLayer object. If rast1=NA (default)
#' then the projection is considered an empty string (""), the raster resolution equals 1 and
#' the raster name that is added by default to the name of the shapefile is "my_raster".
#'
#' @return Does not return a value, saves files to hard drive.
#' @export
#'
#' @examples
#' # Do not run
#' # Consider the bluff/cliff top results from get.toptoe, and the list of rasters
#' # rast1 from clip.rast function. "MyDir" is the full path and the directory where
#' # the files will be saved.
#' #
#' # n6 <- length(top1)
#' # comb.n6 <- combn(1:n6, 1)
#' #
#' # apply(comb.n6, 2, function(x)
#' #   make.shape(top1[[x]], dir.tops="MyDir", file.name="tops", 10, rast1[[x]]))
#'
make.shape <- function(tabl, dir.shape, file.name, smooth.win=NA, rast1=NA){

  if(is.logical(rast1[[1]])==FALSE){
  if(is.character(rast1)) rast <- raster::raster(rast1) else rast <- rast1
  crs1 <- raster::projection(rast)
  res.x<-round(raster::xres(rast),1)
  rast.name <- names(rast)
  } else {
    crs1=""
    res.x=1
    rast.name="my_raster"
  }

  if(is.list(tabl)==TRUE) {

    n <- length(tabl)
    comb1 <- utils::combn(1:n, 1)

    tabl <- apply(comb1, 2, function(x) base::as.data.frame(tabl[[x]]))

    tabl <- base::as.data.frame(data.table::rbindlist(tabl))
  } else tabl <- tabl

  if(is.data.frame(tabl)) tabl <- tabl else tabl <- base::as.data.frame(tabl)

  tabl <- stats::na.omit(tabl)

  dir.shape <- gsub("\\\\", "/", dir.shape)
  dir.create(dir.shape, showWarnings = FALSE)

  shape.sp <- sp::SpatialPointsDataFrame(tabl[,1:2], tabl, proj4string=sp::CRS(crs1))

  raster::shapefile(shape.sp, file=paste(dir.shape, "/", file.name, "_", rast.name, "_points.shp", sep = ""))

  # PolyLine shapefile

  xy <- cbind(tabl[,1], tabl[,2])
  cl <- sp::Line(xy)
  cl1 <- sp::Lines(list(cl), ID = 1)
  row.names(tabl) = seq(1, dim(tabl)[1])

  shape.l <- sp::SpatialLines(list(cl1), proj4string = sp::CRS(crs1))
  shape.ln <- sp::SpatialLinesDataFrame(shape.l, tabl)

  raster::shapefile(shape.ln, file=paste(dir.shape, "/", file.name, "_", rast.name,"_line.shp", sep = ""))

  # do the smoothing of the shape line
  if (!is.na(smooth.win)) {
    rmy <- c(tabl[1,2], zoo::rollmean(tabl[,2], smooth.win), tabl[dim(tabl)[1], 2])
    rmx <- c( tabl[1,1], zoo::rollmean(tabl[,1], smooth.win), tabl[dim(tabl)[1], 1])

    x1y1 <- cbind(rmx, rmy)
    cl1a <- sp::Line(x1y1)
    cl1b <- sp::Lines(list(cl1a), ID = 1)
    row.names(x1y1) <- seq(1, dim(x1y1)[1])
    shape.l1 <- sp::SpatialLines(list(cl1b), proj4string = sp::CRS(crs1))
    x1y1 <- base::as.data.frame(x1y1)
    shape.ln1 <- sp::SpatialLinesDataFrame(shape.l1, x1y1)

    raster::shapefile(shape.ln1, file=paste(dir.shape, "/", file.name, "_", rast.name,
                                            "_smooth_line_window_", round(smooth.win*res.x,0), "m.shp", sep = ""))

  }
}
