#' Create a concave/convex raster of the bluff/cliff face or buffer
#'
#' The function creates along bluff/cliff face profiles (between top and toe) a relative
#' convex (positive values) / concave (negative values) raster, when the bluff parameter is TRUE,
#' otherwise the convex/concave raster is created along the original profile length.
#'
#' @param profs - A list of data.frames, each table is a bluff/cliff profile.
#' Each table in the list has 6 columns in this order with names:
#' xi, yi, zi, id, id1, and ID.prof where:
#' - xi, yi, zi - coordinates, where zi represents elevation (in meters);
#' - id - is a sequential id of rows in the table;
#' - id1 = is the cumulative distance between points on profile;
#' - ID.prof = is the index of the profile, if the list of profile tables has more than
#' one table then this index will identify uniquely the profile.
#' - The profiles are always reoriented to have beach and water to the left and
#' bluff face and land to the right.
#'
#' @param top1 - A list of bluff/cliff top data.frame with one row and same column
#' names as the data.frame from profs list. This is the results from get.toptoe function.
#' - If the bluff parameter is FALSE (default) then top1=NA (default).
#'
#' @param toe1 - A list of bluff/cliff toe data.frame with one row and same column
#' names as the data.frame from profs list. This is the results from get.toptoe function.
#' - If the bluff parameter is FALSE (default) then toe1=NA (default).
#'
#' @param buffdem - Either full path including file name and extension (e.g. .tif for geotiff files)
#' of one DEM raster that was clipped with a buffer or a RasterLayer object.
#'
#' @param bluff - A TRUE/FALSE parameter that indicates if the concavity/convexity is
#' calculated on the original bluff/cliff profile (Default: FALSE) or on bluff/cliff
#' face between top and toe (TRUE).
#' - If bluff=TRUE, then top1 and toe1 parameters are lists of top and toe data respectively,
#' they cannot be NA.
#'
#' @param dir.name - The full path to the directory where the subdirectory "ConvexConcave_rast"
#' will be created to save the raster in geotif format. If the dir.name given does
#' not exist it will be created. If dir.name is NA, the raster will not be saved on the disk.
#'
#' @return A geotiff raster for each processed bluff/cliff buffer.
#' @export
#'
#' @examples
#' # Do not run
#' # Use the prof.noveg list of data.frames and the bluff / cliff top/toe list (top1/toe1) for
#' # obtaining the bluff/cliff face profile. See get.toptoe function example. Or use only
#' # the prof.noveg list of data.frames is the bluff=FALSE for which top1=NA and toe1=NA.
#' #
#' # n10 <- length(prof.noveg)
#' # comb.n10 <- combn(1:n10,1)
#' #
#' # For bluff faces
#' # cc1<- apply(comb.n10, 2, function(x)
#' # iRastCC(prof.noveg[[x]], top1[[x]], toe1[[x]], rast1[[x]], bluff=TRUE, NA))
#' #
#' # For the entire profile
#' # cc2<- apply(comb.n10, 2, function(x)
#' # iRastCC(prof.noveg[[x]], top1=NA, toe1=NA, rast1[[x]], bluff=FALSE, NA))

iRastCC <- function(profs, top1=NA, toe1=NA, buffdem, bluff=FALSE, dir.name=NA){

  n <- length(profs)
  comb1 <-  utils::combn(1:n,1)

  if(bluff==TRUE & !is.na(top1)[[1]] & !is.na(toe1)[[1]]){
    profs1 <- apply(comb1, 2, function(x)
      profCC(profs[[x]], top1, toe1))
  } else profs1<- profs

  len1 <- bLen(profs1)

  profs1 <- apply(comb1, 2, function(x)
    if(is.list(len1)) data.frame(profs1[[x]], len= len1[[x]])
    else data.frame(profs1[[x]], len= len1))

  if(is.character(buffdem)) rast <- raster::raster(buffdem) else rast <- buffdem
  crs1 <- raster::projection(rast)

  xyz <- apply(comb1, 2, function(x)
    data.frame(x=profs1[[x]][,1], y=profs1[[x]][,2], z=profs1[[x]][,7]))

  xyz <- data.table::rbindlist(xyz)

  xyz <- as.matrix(xyz)

  xyz <- base::as.data.frame(xyz[!is.na(xyz[,1]),])

  sp::coordinates(xyz) = ~x+y

  pixels <- sp::SpatialPixelsDataFrame(xyz, tolerance=0.99, proj4string=sp::CRS(crs1), xyz@data)

  ras1 <- raster::raster(pixels[,'z'])
  sp::proj4string(pixels)<- sp::CRS(crs1)

  if(bluff==TRUE){
    r3 <- raster::focal(ras1, w=matrix(1,nrow=3,ncol=3), fun=mean, na.rm = TRUE, NAonly=TRUE)
  } else {
    r3 <- raster::focal(ras1, w=matrix(1,nrow=11,ncol=11), fun=mean, na.rm = TRUE, NAonly=TRUE)
  }

  ras2 <- raster::raster(raster::extent(ras1), resolution=1)
  sp::proj4string(ras2)<- sp::CRS(crs1)

  ras3 <- raster::resample(r3, ras2, method='bilinear')
  sp::proj4string(ras3)<- sp::CRS(crs1)

  if(!is.na(dir.name)){

    dir.name <- gsub("\\\\", "/", dir.name)
    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","ConvexConcave_rast", sep = ""), showWarnings = FALSE)
    dir.rast <- paste(dir.name, "/","ConvexConcave_rast", sep = "")

    nmf <- names(rast)

    raster::writeRaster(ras3, file=paste(dir.rast, "/",nmf, "_ConvexConcave.tif", sep=""),
                        format="GTiff")

  } else dir.name=NA

  return(ras3)
}
