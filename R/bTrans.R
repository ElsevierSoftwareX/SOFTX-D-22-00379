#' Extract transects as SpatialLinesDataFrame
#'
#' This function establishes transects necessary for extracting elevation profiles
#' later on. The transects are within the buffer DEM and do not intersect each other.
#'
#' @param shape.lns1 - Result of make.margin function - a list of 2 line shapefiles that
#' represent the relatively straight edges of the bluff buffer
#'
#' @param dist1 - Distance between the transects - usually at raster resolution.
#' If dist1=NA, dist 1 is equal to raster resolution.
#'
#' @param rast1 - Either full path including file name and extension (e.g. .tif for geotiff files)
#' of one DEM raster that was clipped with a buffer or a RasterLayer object.
#'
#' @param dir.name - The full path to the directory where a bluff transect subdirectory
#' called Transects will be created. The Transects subdirectory will have separate
#' subdirectories for each buffer and a Merged subdirectory. Individual line transects
#' will be saved as line shapefiles, one shapefile for each transect in its respective
#' buffer subdirectory. For rach buffer a merged line shapefile will be saved in the
#' merged subdirectory. If the dir.name given does not exist it will be created.
#' If dir.name is NA, the shapefiles will not be saved on the disk.
#'
#' @return A list of lists with transects as SpatialLinesDataFrame and Lines objects for each buffer.
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
#' #   bTrans(marg1[[x]], dist1=NA, rast1[[x]], NA))
#'
bTrans<- function(shape.lns1, dist1=NA, rast1, dir.name){

  if (inherits(rast1,"RasterLayer")==TRUE) rast <- rast1 else rast <- raster::raster(rast1)
  crs1 <- raster::projection(rast)

  buff_dist<-shape.lns1[[1]]@data$buff_dist
  IDbuff <- shape.lns1[[1]]@data$IDbuff

  if (is.na(dist1)) dist1 <- raster::res(rast)[1] else dist1 <- dist1

  lgth1 <- sp::SpatialLinesLengths(shape.lns1[[1]])
  lgth2 <- sp::SpatialLinesLengths(shape.lns1[[2]])

  if (lgth1 <= lgth2) {
    lsub1 <- shape.lns1[[1]][1,]
    xylsub1 <- sp::coordinates(lsub1)[[1]][[1]]

    rownames(xylsub1)<-NULL
    ln.sub1 <- sp::Line(xylsub1)

    ns1 <- round( (lgth1[1] / dist1), digits=0)
    lsamp1 <- sp::spsample(ln.sub1, n=ns1, type="regular", offset=c(0,0))
    pts1 <- data.frame(x=sp::coordinates(lsamp1)[,1], y=sp::coordinates(lsamp1)[,2])
    pts1 <- data.frame(pts1, id = seq(1, dim(pts1)[1]))
    ###########################################
    lsub2 <- shape.lns1[[2]][1,]
    xylsub2 <- sp::coordinates(lsub2)[[1]][[1]]

    rownames(xylsub2)<-NULL
    ln.sub2 <- sp::Line(xylsub2)

    lsamp2 <- sp::spsample(ln.sub2, n=ns1, type="regular", offset=c(0,0))
    pts2 <- data.frame(x=sp::coordinates(lsamp2)[,1], y=sp::coordinates(lsamp2)[,2])
    pts2 <- data.frame(pts2, id = seq(1, dim(pts2)[1]))
  } else {
    lsub2 <- shape.lns1[[2]][1,]
    xylsub2 <- sp::coordinates(lsub2)[[1]][[1]]

    rownames(xylsub2)<-NULL
    ln.sub2 <- sp::Line(xylsub2)

    ns1 <- round( (lgth2[1] / dist1), digits=0)
    lsamp2 <- sp::spsample(ln.sub2, n=ns1, type="regular", offset=c(0,0))
    pts2 <- data.frame(x=sp::coordinates(lsamp2)[,1], y=sp::coordinates(lsamp2)[,2])
    pts2 <- data.frame(pts2, id = seq(1, dim(pts2)[1]))
    ###########################
    lsub1 <- shape.lns1[[1]][1,]
    xylsub1 <- sp::coordinates(lsub1)[[1]][[1]]

    rownames(xylsub1)<-NULL
    ln.sub1 <- sp::Line(xylsub1)

    lsamp1 <- sp::spsample(ln.sub1, n=ns1, type="regular", offset=c(0,0))
    pts1 <- data.frame(x=sp::coordinates(lsamp1)[,1], y=sp::coordinates(lsamp1)[,2])
    pts1 <- data.frame(pts1, id = seq(1, dim(pts1)[1]))
  }

  n <- dim(pts1)[1]

  # make sure that the points on the shapefiles correspond correctly

  d1 <- sqrt((pts1[1,1]-pts2[1,1])^2 + (pts1[1, 2]-pts2[1, 2])^2)
  d2 <- sqrt((pts1[1, 1]-pts2[n,1])^2 + (pts1[1, 2] - pts2[n, 2])^2)

  if (d1 > d2) {
    pts2 <- pts2[order(pts2[,3], decreasing = TRUE),]
    pts2[,3] <- seq(1, n)
  }

  if (lgth1 >= lgth2) {
    pts1.sh <- data.frame(x=pts2[,1], y = pts2[,2])
    sp::coordinates(pts1.sh) <- c("x", "y")
    sp::proj4string(pts1.sh) <- sp::CRS(crs1)
    pts2.pt <- as.matrix(pts1, ncol = 3)

    pts2.sh <- data.frame(x=pts1[,1], y = pts1[,2])
    sp::coordinates(pts2.sh) <- c("x", "y")
    sp::proj4string(pts2.sh) <- sp::CRS(crs1)
    pts1.pt <- as.matrix(pts2, ncol = 3)

  } else {
    pts1.sh <- data.frame(x=pts1[,1], y = pts1[,2])
    sp::coordinates(pts1.sh) <- c("x", "y")
    sp::proj4string(pts1.sh) <- sp::CRS(crs1)
    pts2.pt <- as.matrix(pts2, ncol = 3)

    pts2.sh <- data.frame(x=pts2[,1], y = pts2[,2])
    sp::coordinates(pts2.sh) <- c("x", "y")
    sp::proj4string(pts2.sh) <- sp::CRS(crs1)
    pts1.pt <- as.matrix(pts1, ncol = 3)

    pts2 <- pts1.pt
    pts1 <- pts2.pt
  }

  n <- dim(pts1)[1]
  pt1.pt <- matrix(nrow = n*2, ncol = 5)
  pt2.pt <- matrix(nrow = n*2, ncol = 3)
  min.idx1 <- 1
  m1<-0
  j1<- 0

  for (i in 1:n) {
    min.index <- which.min(sp::spDistsN1(pts1.sh, pts2.pt[i,1:2]))

    if (min.index - min.idx1 == 0 | min.index-min.idx1 == 1) m1 <- 0 else m1 <- min.index-min.idx1

    if (m1 == 0 | m1 ==1) j1 <- j1 +1 else j1 <- j1 + m1

    m2 <- min.index-min.idx1
    pt1.pt[j1,] <- c(pts1.sh[min.index, , drop = FALSE]@coords[1,], id = i, id.min = min.index, m2)
    pt2.pt[j1,] <- unlist(c(pts1[i,1:2], id = i))
    min.idx2 <- min.idx1
    min.idx1 <- min.index

    if (m1 > 1) {

      for (j2 in (j1-m1+1):(j1-1)){

        pt1.pt[j2,] <- unlist(c(pts2[(min.idx2+1),1:2], id = j2, id.min = j1, m2=9999))
        pt2.pt[j2,] <- unlist(c(pts1[i, 1:2], id=j1))
        min.idx2 <- min.idx2+1

      }
    }
  }

  pt1a.pt <- pt1.pt[!is.na(pt1.pt[,1]),]
  pt2a.pt <- pt2.pt[!is.na(pt2.pt[,1]),]

  colnames(pt1a.pt) <- c("x", "y", "id.pt", "id.min", "idx.diff")
  colnames(pt2a.pt) <- c("x", "y", "id.pt")

  t1 <- table(pt1a.pt[,4])
  s1 <- sort(base::unique(pt1a.pt[,4]))

  pt1a.pt <- cbind(pt1a.pt, sqi = rep(0, dim(pt1a.pt)[1]))

  n2 <- length(s1)
  for (p1 in 1:n2){
    if (!is.null(dim(pt1a.pt[pt1a.pt[,4]==s1[p1],]))) pt1a.pt[pt1a.pt[,4]==s1[p1],][,6] <- seq(1,t1[[p1]]) else pt1a.pt[pt1a.pt[,4]==s1[p1],][6]<- 1
  }

  mod1 <- pt1a.pt[,6] %% 2
  pt1a.pt <- cbind(pt1a.pt, mod1 = mod1)
  pt2a.pt <- cbind(pt2a.pt, pt1a.pt[,6:7])

  pt1a <- pt1a.pt[pt1a.pt[,7]==1,]
  pt2a <- pt2a.pt[pt2a.pt[,5]==1,]

  pts1 <- pt1a[,1:3]
  pts2 <- pt2a[,1:3]

  pts1[,3] <- seq(1,dim(pts1)[1])
  pts2[,3] <- seq(1, dim(pts2)[1])

  n <- dim(pts1)[1]
  comb1 <-  utils::combn(1:n,1)

  tabl <- apply(comb1, 2, function(x) data.frame(x=c(pts1[x,1], pts2[x,1]),
                                                 y = c(pts1[x,2], pts2[x,2]), ID.buff = rep(IDbuff, 2),buff.dist=rep(buff_dist,2)))

  cl <- apply(comb1, 2, function(x) sp::Line(cbind(tabl[[x]][,1], tabl[[x]][,2])))

  cl1 <- apply(comb1, 2, function(x) sp::Lines(cl[[x]], ID = x))

  shape.l <- apply(comb1, 2, function(x) sp::SpatialLines(list(cl1[[x]]), proj4string = sp::CRS(crs1)))

  shape.ln <- apply(comb1, 2, function(x) sp::SpatialLinesDataFrame(shape.l[[x]], tabl[[x]][1,], match.ID=FALSE))

  shape.l1 <-sp:: SpatialLines(unlist(cl1), proj4string = sp::CRS(crs1))

  tabl1 <- data.frame(x1 = pts1[,1], y1 = pts1[,2], id1 = pts1[,3], x2 = pts2[,1],
                      y2 = pts2[,2], id2 = pts2[,3], ID.buff=IDbuff, buff.dist=buff_dist)

  shape.mln <- sp::SpatialLinesDataFrame(shape.l1, tabl1, match.ID=FALSE)

  if (is.na(dir.name)){
    dir.name <- NA
  } else {
    n <- dim(pts1)[1]
    comb1 <-  utils::combn(1:n,1)

    dir.name <- gsub("\\\\", "/", dir.name)
    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Transects", sep = ""), showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Transects", "/", "Merged",sep = ""), showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Transects", "/", "TransBuff", IDbuff, sep = ""), showWarnings = FALSE)
    dir.transm <- paste(dir.name, "/","Transects", "/", "Merged",sep = "")
    dir.IDbuff <- paste(dir.name, "/","Transects", "/", "TransBuff", IDbuff, sep = "")

    apply(comb1, 2, function(x)
      raster::shapefile(shape.ln[[x]], file = paste(dir.IDbuff, "/trans",x,"_buff", IDbuff,
                                                    "_", buff_dist, "m.shp", sep="")))

    raster::shapefile(shape.mln, file=paste(dir.transm, "/merged_transects_buff", IDbuff,
                                            "_", buff_dist, "m.shp", sep=""))
  }

  out.all <- list(shape.ln=shape.ln, Lines = cl1)
  return(out.all)

}
