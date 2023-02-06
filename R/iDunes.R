#' Identify dune crests and troughs on beach profiles
#'
#' The function gets all crests and troughs without knowing a priori the number of dunes.
#' All crests and troughs get a count or frequency to show how many times
#' the same point was identified since the search is done within percentiles
#' bands or any other type of data splits. The higher the frequency number the
#' more prominent the crest / trough is. The beach profile needs to have at least 6
#' data points, otherwise that profile will be ignored.
#'
#' @param profs1 - A list of data.frames, each table is a beach profile.
#' Each table in the list has only 6 columns in this order with names:
#' xi, yi, zi, id, id1, and ID.prof where:
#' - xi, yi, zi - coordinates, where zi represents elevation (in meters);
#' - id - is a sequential id of rows in the table;
#' - id1 = is the cumulative distance between points on profile;
#' - ID.prof = is the index of the profile, if the list of profile tables has more than
#' one table then this index will identify uniquely the profile.
#'
#' @param splits - A lost that shows how to split the profile data:
#' If length(splits)=0 then splits are percentiles 5, 10, 20 ..... 90 and 95%
#' of elevations along the profile.
#'
#' - If not - give a list with first element the function name (as character string) such as
#' "percentiles" or "sequence", or a user defined function name.
#' #' For "percentiles" or "sequence" the list has a second element that represents
#' probalities in (0, 1) for percentile mode ,(see quantiles function), or the
#' length for sequence mode (see seq function).
#'
#' - If the function is user defined, the function is applied to the detrended elevation values and
#' should results in a vector that gives break values in the space of the elevation detrended values.
#'
#' @param rast1 - Either full path including file name and extension (e.g. .tif for geotiff files)
#' of one DEM raster that was clipped with a buffer or a RasterLayer object.
#'
#' @param dir.name - The full path to the directory where the subdirectory Dunes will be created
#' to save point shapefiles for dune crests and troughs. If the dir.name given does
#' not exist it will be created. If dir.name is NA, the shapefiles will not be saved on the disk.
#'
#' @return A list of lists of data.frames. Each list in the list of lists has 2 elements,
#' the first represents the data.frame for dune crests and the second represents the
#' data.frame for dune troughs.
#' @export
#'
#' @examples
#' # Do not run
#' # Use the prof.noveg list of data.frames and the bluff / cliff toes list (toe1) for
#' # obtaining the profs1 parameter. See get.toptoe function example. Or use a list
#' # of data.frames representing beach priofiles.
#' #
#' # n7 <- length(prof.noveg)
#' # comb.n7 <- combn(1:n7,1)
#' #
#' # profs1 <- apply(comb.n7, 2, function(x)
#' #   {n8<- length(prof.noveg[[x]]);
#' #    comb.n8 <- combn(1:n8,1);
#' #    apply(comb.n8, 2, function(i)
#' #      iBeach(prof.noveg[[x]][[i]], toe1[[x]]))})
#' #
#' # n9 <- length(profs1)
#' # comb.n9 <- combn(1:n9, 1)
#' #
#' # dunes <- apply(comb.n9, 2, function(x)
#' #   iDunes(profs1[[x]], splits=list(), rast1[[x]], NA))
#' #
#' # dune.crest <- dunes[[1]]
#' # dune.trough <- dunes[[2]]
#'
iDunes <- function(profs1, splits=list(), rast1, dir.name=NA){

  if(is.character(rast1)) rast <- raster::raster(rast1) else rast <- rast1
  crs1 <- raster::projection(rast)
  rast.name <- names(rast)

  try(if(is.list(profs1)& is.data.frame(profs1)==TRUE) stop ("profile cannot be a data.frame"))

  n <- length(profs1)
  comb1 <-  utils::combn(1:n,1)

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  p2 <- which(p1>5)

  profs1 <- profs1[p2]

  # this is repeated in case any profiles were eliminated
  n <- length(profs1)
  comb1 <-  utils::combn(1:n,1)

  profs1 <- apply(comb1, 2, function(x) if(profs1[[x]][1,3]==profs1[[x]][2,3])
    profs1[[x]][-2,] else profs1[[x]])

  profs1 <- apply(comb1, 2, function(x)
    data.frame(profs1[[x]][,1:3], id=seq(1, dim(profs1[[x]])[1]), profs1[[x]][,5:6]))

  cnames <- c("xi", "yi", "zi", "id", "id1", "ID.prof")

  profs1<- lapply(profs1, stats::setNames, cnames)

  len1 <- bLen(profs1)

  profs1 <- apply(comb1, 2, function(x)
    if(is.list(len1)) data.frame(profs1[[x]], len1=len1[[x]])
    else data.frame(profs1[[x]], len1=len1))

  profs2 <- profs1

  p <- apply(comb1, 2, function(x) dim(profs2[[x]])[1])

  profs2 <- apply(comb1, 2, function(x) profs2[[x]][c(-1, -p[x]),])

  if (length(splits)==0){
    prob1 <- c(0.05, seq(0.1,0.9, 0.1), 0.95)
    vals <- apply(comb1, 2, function(x) stats::quantile(profs2[[x]]$len1, prob1, na.rm = TRUE))
  } else {

    if (splits[[1]]=="percentiles"){
      prob1 <- splits[[2]]
      vals <- apply(comb1, 2, function(x) stats::quantile(profs2[[x]]$len1, prob1, na.rm = TRUE))
    } else {
      if(splits[[1]]=="sequence"){
        if(length(splits[[2]])>1) l1 <- length(splits[[2]]) else l1 <- splits[[2]]
        vals <- apply(comb1, 2, function(x)
          seq(min(profs2[[x]]$len1)[1], max(profs2[[x]]$len1)[1], length=l1))
      } else {
        vals <- apply(comb1, 2, function(x) splits[[1]](profs2[[x]]$len1))
      }
    }
  }

  ### get all the toes

  mint <- apply(comb1, 2, function(x) get.mint(profs2[[x]], vals[,x]))
  mint <- apply(comb1, 2, function(x) if(mint[[x]][1,4]<4 & dim(mint[[x]])[1]>2)
    mint[[x]][-1,] else mint[[x]])

  ### do similar to get all tops now
  profs2 <- profs1

  p <- apply(comb1, 2, function(x) dim(profs2[[x]])[1])

  profs2 <- apply(comb1, 2, function(x) profs2[[x]][c(-1, -p[x]),])

  maxt <- apply(comb1, 2, function(x) get.maxt(profs2[[x]], vals[,x]))
  maxt <- apply(comb1, 2, function(x) if(maxt[[x]][1,4]<4 & dim(maxt[[x]])[1]>2)
    maxt[[x]][-1,] else maxt[[x]])

  if (is.na(dir.name)){
    dir.name <- NA
  } else {

    dir.name <- gsub("\\\\", "/", dir.name)
    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Dunes", sep = ""), showWarnings = FALSE)
    dir.dunes <-paste(dir.name, "/","Dunes", sep = "")

    n.maxt <- length(maxt)
    n.mint <- length(mint)

    comb.maxt <- utils::combn(1:n.maxt,1)
    comb.mint <- utils::combn(1:n.mint, 1)

    maxt1 <- apply(comb.maxt, 2, function(x) base::as.data.frame(maxt[[x]]))
    mint1 <- apply(comb.mint, 2, function(x) base::as.data.frame(mint[[x]]))

    maxt1 <- base::as.data.frame(data.table::rbindlist(maxt1))
    mint1 <- base::as.data.frame(data.table::rbindlist(mint1))

    maxt1 <- stats::na.omit(maxt1)
    mint1 <- stats::na.omit(mint1)

    # build spatial objects that can be saved as ESRI shapefiles
    shape.maxt <- sp::SpatialPointsDataFrame(maxt1[,1:2], maxt1, proj4string=sp::CRS(crs1))

    shape.mint <- sp::SpatialPointsDataFrame(mint1[,1:2], mint1, proj4string=sp::CRS(crs1))

    raster::shapefile(shape.maxt, file=paste(dir.dunes, "/dTops _", rast.name, "_points.shp", sep = ""))
    raster::shapefile(shape.mint, file=paste(dir.dunes, "/dToes _", rast.name, "_points.shp", sep = ""))

  }

  rez <- list(peaks = maxt, troughs=mint)

  return(rez)
}
