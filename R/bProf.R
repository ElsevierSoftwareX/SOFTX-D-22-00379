#' Extract elevation profiles
#'
#' This function extracts elevation profiles along the transects within the DEM
#' buffers. The function extracts one single profile in approximately 2 seconds in average
#' if the profile is also saved on disk.
#'
#' @param lns1 - Is a list of class "Lines" and is the result (the 2nd list term) of the
#' bTrans function for each considered buffer.
#'
#' @param rast1 - Either full path including file name and extension (e.g. .tif for geotiff files)
#' of one DEM raster that was clipped with a buffer or a RasterLayer object.
#'
#' @param buff1 - Either full path including file name and extension .shp of one
#' polygon buffer shapefile, or a SpatialPolygonsDataFrame object that contains
#' one polygon shapefile. This is usually the result from bBluff function, one
#' object from the resulting list.
#'
#' @param dir.name - The full path to the directory where a bluff profile subdirectory
#'called Profiles will be created.The Profiles subdirectory will have separate
#' subdirectories for each buffer. Individual comma delimited tables (*.csv) will be
#' saved for each extracted profile. If the dir.name given does not exist it will be created.
#' If dir.name is NA, the *.csv tables will not be saved on the disk.
#'
#' @return A list of lists of data.frames, each table is a bluff/cliff profile.
#' Each table in the list has 6 columns in this order with names:
#' xi, yi, zi, id, id1, and ID.prof where:
#' - xi, yi, zi - coordinates, where zi represents elevation (in meters);
#' - id - is a sequential id of rows in the table;
#' - id1 = is the cumulative distance between points on profile;
#' - ID.prof = is the index of the profile, if the list of profile tables has more than
#' one table then this index will identify uniquely the profile.
#' - The profiles are always reoriented to have beach and water to the left and
#' bluff face and land to the right.
#' @export
#'
#' @examples
#' # Do not run
#' # coast1 <- system.file("extdata", "shore_line.shp", package = "iBluff")
#' # buff1 <- bBluff(coast1, NA)
#' # shore1 <- bCoast(coast1, NA)
#' # dem1 <- system.file("extdata", "dem_demo.tif", package = "iBluff")
#' # rast1 <- clip.rast(dem1, buff1,NA)
#' #
#' # n <- length(shore1)
#' # comb.n <-  combn(1:n,1)
#' # marg1 <- apply(comb.n, 2, function(x)
#' #   make.margins(buff1[[x]], shore1[[x]], rast1[[x]], NA))
#' #
#' # n <- length(marg1)
#' # comb.n <-  combn(1:n,1)
#' # trans1.all <- apply(comb.n, 2, function(x)
#' #    bTrans(marg1[[x]], dist1=NA, rast1[[x]], NA))
#' #
#' # n2 <- length(trans1.all)
#' # comb.tr <- combn(1:n2, 1)
#' # lns1 <- apply(comb.tr, 2, function(x) trans1.all[[x]][[2]])
#' #
#' # n3 <- length(lns1)
#' # comb.n3 <- combn(1:n3,1)
#' # profs <- apply(comb.n3, 2, function(x)
#' #    bProf(lns1[[x]], rast1[[x]], buff1[[x]], NA))

bProf <- function(lns1, rast1, buff1, dir.name){

  if (inherits(rast1,"RasterLayer")==TRUE) rast <- rast1 else rast <- raster::raster(rast1)
  crs1 <- raster::projection(rast)

  dist1 <- raster::res(rast)[1]

  if(inherits(buff1, "SpatialPolygonsDataFrame")==TRUE) buff2 <- buff1 else buff2 <- shape.read(buff1)
  IDbuff <- buff2@data[1,1]
  buff.dist <- buff2@data[1,2]

  n <- length(lns1)

  comb1 <-  utils::combn(1:n,1)

  xy.p1<- lapply(comb1, function(x) sp::coordinates(lns1[[x]])[[1]])

  m1 <- apply(comb1, 2, function(x)
    if(xy.p1[[x]][1,1]-xy.p1[[x]][2,1] != 0)
      m1 <- (xy.p1[[x]][1,2]-xy.p1[[x]][2,2])/(xy.p1[[x]][1,1]-xy.p1[[x]][2,1])
    else m1 <- (xy.p1[[x]][1,2]-xy.p1[[x]][2,2])/ 2.6e-07)

  b1 <- apply(comb1, 2, function(x) xy.p1[[x]][1, 2] - (m1[x]*xy.p1[[x]][1,1]))

  a <- apply(comb1, 2, function(x) if(!is.na(xy.p1[[x]][1,1]))
    dist1 * cos(atan(m1[x]))else a <- NA)

  a1 <- apply(comb1, 2, function(x) if (a[x]==0) a1 <- 2.6e-07 else a1 <- a[x])

  xi <- apply(comb1, 2, function(x)
    if(is.na(a1[x])) xi<- NA else
      if(xy.p1[[x]][1,1] < xy.p1[[x]][2,1]) xi <- seq(xy.p1[[x]][1,1], xy.p1[[x]][2,1], a1[x])
    else xi <- seq(xy.p1[[x]][1,1], xy.p1[[x]][2,1], -a1[x]))

  if(class(xi)[1]=="matrix") xi <- lapply(seq_len(ncol(xi)), function(x) xi[,x]) else xi<- xi

  yi <- apply(comb1, 2, function(x) xi[[x]]*m1[x]+b1[x])

  if(class(yi)[1]=="matrix") yi<- lapply(seq_len(ncol(yi)), function(x) yi[,x]) else yi<- yi

  p1 <- apply(comb1, 2, function(x) data.frame(xi=xi[[x]], yi=yi[[x]]))

  zi <- apply(comb1, 2, function(x) if (!is.na(a1[x])) raster::extract(rast, p1[[x]]) else NA)

  if(class(zi)[1]=="matrix") zi <- lapply(seq_len(ncol(zi)), function(x) zi[,x]) else zi<- zi


  pa <- apply(comb1, 2, function(x) data.frame(p1[[x]], zi[[x]]))

  pa <- apply(comb1, 2, function(x) if (!is.na(a1[x])) pa[[x]][!is.na(pa[[x]][,3]),]
              else data.frame(xi=NA, yi=NA, zi=NA))

  pa <- apply(comb1, 2, function(x) if (dim(pa[[x]])[1]>5) pa[[x]]<- pa[[x]]
              else pa[[x]]<- data.frame(xi=NA, yi=NA, zi=NA))

  id3 <- apply(comb1, 2, function(x) if (dim(pa[[x]])[1]>5) seq(1, dim(pa[[x]])[1]) else NA)

  pa <- apply(comb1, 2, function(x)
    data.frame(xi=pa[[x]][,1], yi=pa[[x]][,2], zi=pa[[x]][,3], id3=id3[[x]]))

  p <- apply(comb1, 2, function(x) dim(pa[[x]])[1])

  pa <- apply(comb1, 2, function(x) if (pa[[x]][1,3] > pa[[x]][p[x],3]) pa[[x]][base::order(pa[[x]][,4], decreasing = TRUE),]
              else pa[[x]])

  id <- apply(comb1, 2, function(x) if (dim(pa[[x]])[1]>5) seq(1, dim(pa[[x]])[1]) else NA)

  id1 <- apply(comb1, 2, function(x) if (dim(pa[[x]])[1]>5) seq(1,by = dist1, length.out=dim(pa[[x]])[1]) else NA)

  pa <- apply(comb1, 2, function(x)
    data.frame(xi=pa[[x]][,1], yi=pa[[x]][,2], zi=pa[[x]][,3], id=id[[x]], id1=id1[[x]], ID.prof=x))

  if (is.na(dir.name)){
    dir.name <- NA
  } else {
    n <- length(pa)

    comb1 <-  utils::combn(1:n,1)

    dir.name <- gsub("\\\\", "/", dir.name)
    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Profiles", sep = ""), showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Profiles", "/", "ProfBuff", IDbuff, sep = ""), showWarnings = FALSE)
    dir.profbuff <- paste(dir.name, "/","Profiles", "/", "ProfBuff", IDbuff, sep = "")

    apply(comb1, 2, function(x)
      utils::write.csv(pa[[x]], file=paste(dir.profbuff, "/prof", x, "_buff", IDbuff,
                                           "_", buff.dist, "m.csv", sep=""), row.names = FALSE))
  }
  return(pa)
}
