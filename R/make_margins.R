#' Extracting the sides / margins of a polygon shapefiles as line shapefiles
#'
#' This function transform a polygon buffer shapefile into a line shapefile and
#' split it in 4 lines, the 2 rounded ends and the 2 usually linear sides. The
#' 2 sides are saved as line shapefiles.
#'
#' @param buff1 - Either full path including file name and extension .shp of one
#' polygon buffer shapefile, or a SpatialPolygonsDataFrame object that contains
#' one polygon shapefile. This is usually the result from bBluff function, one
#' object from the resulting list.
#'
#' @param shore1 - Either full path including file name and extension .shp of one
#' line shapefile, or a SpatialLinesDataFrame object that contains only one line
#' shapefile. This is usually the result from bCoast function, one object from the
#' resulting list.
#'
#' @param rast1 - Either full path including file name and extension (e.g. .tif
#' for geotiff files) of one DEM raster that was clipped with a buffer or a
#' RasterLayer object.
#'
#' @param dir.name - The full path to the directory where a Margins subdirectory
#' will be created and the individual buffers sides will be saved as line shapefiles,
#' one shapefile for each side. If the dir.name given does not exist it will be
#' created. If dir.name is NA, the shapefiles will not be saved on the disk.
#'
#' @return A list of lists with two SpatialLinesDataFrame objects each.
#' @export
#'
#' @examples
#' coast1 <- system.file("extdata", "shore_line.shp", package = "iBluff")
#' buff1 <- bBluff(coast1, NA)
#' shore1 <- bCoast(coast1, NA)
#' dem1 <- system.file("extdata", "dem_demo.tif", package = "iBluff")
#' rast1 <- clip.rast(dem1, buff1,NA)
#'
#' n <- length(shore1)
#' comb.n <-  combn(1:n,1)
#' marg1 <- apply(comb.n, 2, function(x)
#'   make.margins(buff1[[x]], shore1[[x]], rast1[[x]], NA))

make.margins <- function(buff1, shore1, rast1, dir.name){

  rgeos::set_RGEOS_CheckValidity(0L)

  if (inherits(rast1,"RasterLayer")==TRUE) rast <- rast1 else rast <- raster::raster(rast1)
  crs1 <- raster::projection(rast)

  if(inherits(shore1,"SpatialLinesDataFrame")==TRUE) lev <- shore1 else lev <- shape.read(shore1)
  dist.buff <- lev@data[,2]
  ID1 <- lev@data[,1]
  ln.lev <- lev@lines
  lev.ln <- sp::SpatialLines(ln.lev, sp::CRS(crs1))
  xy.levln <- sp::coordinates(lev.ln)[[1]][[1]]

  xy <- as.data.frame(xy.levln)
  colnames(xy) <- c("x", "y")
  n <- dim(xy)[1]

  if(inherits(buff1, "SpatialPolygonsDataFrame")==TRUE) buff2 <- buff1 else buff2 <- shape.read(buff1)
  buff <- methods::as(buff2, "SpatialLinesDataFrame")

  ln.buff <- buff@lines
  buff.ln <- sp::SpatialLines(ln.buff, sp::CRS(crs1))

  if (length(sp::coordinates(buff.ln)[[1]]) ==1) {

    xy.buffln <- sp::coordinates(buff.ln)[[1]][[1]]

    colnames(xy.buffln) <- c("x", "y")
    xybuff <- as.data.frame(xy.buffln)

    # for the first 2 vertex points on the "shore" line "draw" a perpendicular line
    # on that segment on the first vertex and find out the closest vertex on the buffer
    # line to the point of intersection between the buffer and the perpendicular line.
    # do the same thing on the last vertex on the shore line

    m <- (xy[1,2]-xy[2,2])/(xy[1,1]-xy[2,1])

    if(m!=Inf){
      a.m <- atan(m) # angle in radians
      b.m <-( pi/2)+a.m
      m1 <- tan(b.m) #slope of perpendicular line
      b <- xy[1,2]-(m*xy[1,1])
      b1 <- xy[1,2]-(m1*xy[1,1])

      d1 <- (dist.buff+10)* cos(atan(m1))
      xa1 <- xy[1,1]-d1
      xa2 <- xy[1,1]+d1
      ya1 <- m1*xa1+b1
      ya2 <- m1*xa2+b1
    } else{
      xa1 <- min(xybuff[,1])-(dist.buff+5)
      xa2 <- max(xybuff[,1])+dist.buff+5
      ya1 <- xy[1,2]
      ya2 <- xy[1,2]
    }

    pln <- data.frame(x=c(xa1, xa2), y = c(ya1, ya2))
    cl1a <- sp::Line(pln)
    cl1b <- sp::Lines(list(cl1a), ID = 1)
    row.names(pln) <- seq(1, dim(pln)[1])
    shape.l1 <- sp::SpatialLines(list(cl1b), proj4string = sp::CRS(crs1))
    pln <- as.data.frame(pln)
    pln <- sp::SpatialLinesDataFrame(shape.l1, pln)

    pln <- sp::spTransform(x = pln, CRSobj=crs1)
    buff<- sp::spTransform(x = buff, CRSobj=crs1)

    p1 <- rgeos::gIntersection(buff, pln)
    wd <- dist.buff

    if (is.null(p1)) {
      while (is.null(p1)){
        wd <- wd+1
        pt1 <- data.frame(x = xy[1, 1], y = xy[1, 2])
        sp::coordinates(pt1) <- c("x", "y")
        sp::proj4string(pt1) <- sp::CRS(crs1)
        pt1.cpoly <- rgeos::gBuffer(pt1, width = wd, byid = TRUE)
        xy.poly <- pt1.cpoly@polygons[[1]]@Polygons[[1]]@coords
        xy.poly <- as.data.frame(xy.poly)
        cpoly1a <- sp::Line(xy.poly)
        cpoly1b <- sp::Lines(list(cpoly1a), ID = 1)
        row.names(xy.poly) <- seq(1, dim(xy.poly)[1])
        shape.poly1 <- sp::SpatialLines(list(cpoly1b), proj4string = sp::CRS(crs1))
        poly.ln <- sp::SpatialLinesDataFrame(shape.poly1, xy.poly)
        buff<- sp::spTransform(x = buff, CRSobj=crs1)
        poly.ln<- sp::spTransform(x = poly.ln, CRSobj=crs1)
        p1 <- rgeos::gIntersection(buff, poly.ln)
      }
    }

    xy.p1 <- p1@coords
    wd <- dist.buff

    if (dim(xy.p1)[1] == 1) {
      pt1 <- data.frame(x = xy[1, 1], y = xy[1, 2])
      sp::coordinates(pt1) <- c("x", "y")
      sp::proj4string(pt1) <- sp::CRS(crs1)
      pt1.cpoly <- rgeos::gBuffer(pt1, width = dist.buff+1, byid = TRUE)
      xy.poly <- pt1.cpoly@polygons[[1]]@Polygons[[1]]@coords
      xy.poly <- as.data.frame(xy.poly)
      cpoly1a <- sp::Line(xy.poly)
      cpoly1b <- sp::Lines(list(cpoly1a), ID = 1)
      row.names(xy.poly) <- seq(1, dim(xy.poly)[1])
      shape.poly1 <- sp::SpatialLines(list(cpoly1b), proj4string = sp::CRS(crs1))
      poly.ln <- sp::SpatialLinesDataFrame(shape.poly1, xy.poly)
      buff<- sp::spTransform(x = buff, CRSobj=crs1)
      poly.ln<- sp::spTransform(x = poly.ln, CRSobj=crs1)
      p1 <- rgeos::gIntersection(buff, poly.ln)
      xy.p1 <- p1@coords
    }

    k <- dim(xy.p1)[1]

    if (dim(xy.p1)[1] > 2){
      t1 <- matrix(ncol = 3, nrow = dim(xy.p1)[1], byrow = TRUE)
      colnames(t1) <- c("max.dist", "pt1.id", "pt2.id")

      comb.3 <- utils::combn(1:k, 1)
      dist.id <- apply(comb.3, 2, function(x)
        which.max(apply(xy.p1, 1, function(i)
          sqrt((i[1]-xy.p1[x,][[1]])^2 + (i[2]-xy.p1[x,][[2]])^2))))

      dist.pts <- apply(comb.3, 2, function(x)
        apply(xy.p1, 1, function(i)
          sqrt((i[1]-xy.p1[x,][[1]])^2 + (i[2]-xy.p1[x,][[2]])^2)))

      t1 <- t(apply(comb.3, 2, function(x) c(dist.pts[,x][[dist.id[[x]]]], x, dist.id[[x]])))

      max.pda <- t1[t1[,1]==max(t1[,1]),][1,]
      pt1.id <- max.pda[[2]]
      pt2.id <- max.pda[[3]]
      xyp1 <- xy.p1[pt1.id,]
      id1 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp1[[1]])^2 + (x[2]-xyp1[[2]])^2)))
      xyp2 <- xy.p1[pt2.id,]
      id2 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp2[[1]])^2 + (x[2]-xyp2[[2]])^2)))
    } else {
      xyp1 <- xy.p1[1,]
      id1 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp1[[1]])^2 + (x[2]-xyp1[[2]])^2)))
      xyp2 <- xy.p1[k,]
      id2 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp2[[1]])^2 + (x[2]-xyp2[[2]])^2)))
    }

    # for the last 2 points on the levee line

    m <- (xy[(n-1),2]-xy[n,2])/(xy[(n-1),1]-xy[n,1])

    if(m!=Inf){
      a.m <- atan(m) # angle in radians
      b.m <-( pi/2)+a.m
      m1 <- tan(b.m) #slope of perpendicular line
      b <- xy[n,2]-(m*xy[n,1])
      b1 <- xy[n,2]-(m1*xy[n,1])

      d1 <- (dist.buff+10)* cos(atan(m1))
      xb1 <- xy[n,1]-d1
      xb2 <- xy[n,1]+d1
      yb1 <- m1*xb1+b1
      yb2 <- m1*xb2+b1
    } else{
      xb1 <- min(xybuff[,1])-(dist.buff+10)
      xb2 <- max(xybuff[,1])+dist.buff+10
      yb1 <- xy[n,2]
      yb2 <- xy[n,2]
    }

    plnb <- data.frame(x=c(xb1, xb2), y = c(yb1, yb2))
    cl1ab <- sp::Line(plnb)
    cl1bb <- sp::Lines(list(cl1ab), ID = 1)
    row.names(plnb) <- seq(1, dim(plnb)[1])
    shape.l1b <- sp::SpatialLines(list(cl1bb), proj4string = sp::CRS(crs1))
    plnb <- as.data.frame(plnb)
    plnb <- sp::SpatialLinesDataFrame(shape.l1b, plnb)

    buff<- sp::spTransform(x = buff, CRSobj=crs1)
    plnb<- sp::spTransform(x = plnb, CRSobj=crs1)

    p1b <- rgeos::gIntersection(buff, plnb)

    wd <- dist.buff

    if (is.null(p1b)) {
      while (is.null(p1b)){
        wd <- wd+1
        pt1b <- data.frame(x = xy[n, 1], y = xy[n, 2])
        sp::coordinates(pt1b) <- c("x", "y")
        sp::proj4string(pt1b) <- sp::CRS(crs1)
        pt1b.cpoly <- rgeos::gBuffer(pt1b, width = wd, byid = TRUE)
        xy.polyb <- pt1b.cpoly@polygons[[1]]@Polygons[[1]]@coords
        xy.polyb <- as.data.frame(xy.polyb)
        cpoly1a <- sp::Line(xy.polyb)
        cpoly1b <- sp::Lines(list(cpoly1a), ID = 1)
        row.names(xy.polyb) <- seq(1, dim(xy.polyb)[1])
        shape.poly1b <- sp::SpatialLines(list(cpoly1b), proj4string = sp::CRS(crs1))
        poly.lnb <- sp::SpatialLinesDataFrame(shape.poly1b, xy.polyb)

        buff<- sp::spTransform(x = buff, CRSobj=crs1)
        poly.lnb<- sp::spTransform(x = poly.lnb, CRSobj=crs1)

        p1b <- rgeos::gIntersection(buff, poly.lnb)
      }
    }

    xy.p1b <- p1b@coords

    if (dim(xy.p1b)[1] == 1) {
      pt1b <- data.frame(x = xy[n, 1], y = xy[n, 2])
      sp::coordinates(pt1b) <- c("x", "y")
      sp::proj4string(pt1b) <- sp::CRS(crs1)
      pt1b.cpoly <- rgeos::gBuffer(pt1b, width = dist.buff+1, byid = TRUE)
      xy.polyb <- pt1b.cpoly@polygons[[1]]@Polygons[[1]]@coords
      xy.polyb <- as.data.frame(xy.polyb)
      cpoly1a <- sp::Line(xy.polyb)
      cpoly1b <- sp::Lines(list(cpoly1a), ID = 1)
      row.names(xy.polyb) <- seq(1, dim(xy.polyb)[1])
      shape.poly1b <- sp::SpatialLines(list(cpoly1b), proj4string = sp::CRS(crs1))
      poly.lnb <- sp::SpatialLinesDataFrame(shape.poly1b, xy.polyb)

      buff<- sp::spTransform(x = buff, CRSobj=crs1)
      poly.lnb<- sp::spTransform(x = poly.lnb, CRSobj=crs1)

      p1b <- rgeos::gIntersection(buff, poly.lnb)
      xy.p1b <- p1b@coords
    }

    k <- dim(xy.p1b)[1]

    if (dim(xy.p1b)[1] > 2){
      t1 <- matrix(ncol = 3, nrow = dim(xy.p1b)[1], byrow = TRUE)
      colnames(t1) <- c("max.dist", "pt1.id", "pt2.id")

      n3 <- dim(xy.p1b)[1]
      comb.3 <- utils::combn(1:n3, 1)
      dist.id <- apply(comb.3, 2, function(x)
        which.max(apply(xy.p1b, 1, function(i)
          sqrt((i[1]-xy.p1b[x,][[1]])^2 + (i[2]-xy.p1b[x,][[2]])^2))))

      dist.pts <- apply(comb.3, 2, function(x)
        apply(xy.p1b, 1, function(i)
          sqrt((i[1]-xy.p1b[x,][[1]])^2 + (i[2]-xy.p1b[x,][[2]])^2)))

      t1 <- t(apply(comb.3, 2, function(x) c(dist.pts[,x][[dist.id[[x]]]], x, dist.id[[x]])))

      max.pdb <- t1[t1[,1]==max(t1[,1]),][1,]
      pt1.id <- max.pdb[[2]]
      pt2.id <- max.pdb[[3]]
      xyp1 <- xy.p1b[pt1.id,]
      id3 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp1[[1]])^2 + (x[2]-xyp1[[2]])^2)))
      xyp2 <- xy.p1b[pt2.id,]
      id4 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp2[[1]])^2 + (x[2]-xyp2[[2]])^2)))
    } else {
      xyp1 <- xy.p1b[1,]
      id3 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp1[[1]])^2 + (x[2]-xyp1[[2]])^2)))
      xyp2 <- xy.p1b[k,]
      id4 <- which.min(apply(xybuff, 1, function(x) sqrt((x[1]-xyp2[[1]])^2 + (x[2]-xyp2[[2]])^2)))
    }

    #the buffer line has the first vertex somewhere and it may not coincide with one of the
    #vertexes identified by the intersection. So i have to move the vertexes around to start
    # the line shapefile with the vertex that has the minimum id number, and the vertexes are nicely
    # respecting the segments between those IDs numbers, so i can clip the buff line in 4 line shapefile.

    xybuff$type <- 0
    xybuff[c(id1, id2, id3, id4), 3] <- 1
    n <- dim(xybuff)[1]
    xybuff1 <- rbind(xybuff[c(min(id1, id2, id3, id4):n),], xybuff[c(1: min(id1, id2, id3, id4)),])
    xybuff1$id <- seq(1, dim(xybuff1)[1])
    xybuff1$buff_dist <- dist.buff
    xybuff1$IDbuff <- ID1

    ids <- xybuff1[xybuff1[,3]==1,][,4]

    nd <- length(ids)
    ids1 <- matrix(c(ids[1:(nd-1)], ids[2:nd]), nrow=4, ncol=2)

    xyc1 <- apply(ids1, 1, function(x) cbind(xybuff1[c(x[1]:x[2]),c(1,2)]))

    comb1 <-  utils::combn(1:length(xyc1),1)

    cla <- apply(comb1, 2, function(x) sp::Line(xyc1[x]))
    cla1 <- apply(comb1, 2, function(x) sp::Lines(list(cla[[x]]), ID=1))

    row.names(xybuff1) = seq(1, dim(xybuff1)[1])

    shape.l<- apply(comb1, 2, function(x) sp::SpatialLines(list(cla1[[x]]), sp::CRS(crs1)))

    shape.ln <- apply(comb1, 2, function(x)
      sp::SpatialLinesDataFrame(shape.l[[x]], xybuff1[ids[x],], match.ID=FALSE))

    #### identify the 2 shapefiles that need to be "deleted" that means eliminate
    # the rounded sides of the buffer
    ######################################

    n <- dim(xy)[1]

    if (xy[1,1]-xy[2,1] == 0) m<- 0 else  m <- (xy[1,2]-xy[2,2])/(xy[1,1]-xy[2,1])

    if(m!=0){
      xa1 <- xy[1,1]-(dist.buff+10)
      b <- xy[1,2]-(m*xy[1,1])
      ya1 <- m * xa1 +b

      xa2 <- xy[1,1]+(dist.buff+10)
      ya2 <- m * xa2 +b
    } else {
      if(xy[1,1]-xy[2,1] == 0 & m==0){
        xa1 <- xy[1,1]
        ya1 <- xy[1,2]-(dist.buff+10)

        xa2 <- xy[1,1]
        ya2 <- xy[1,2]+(dist.buff+10)
      } else {
        if(xy[1,2]-xy[2,2]==0 & m==0){
          xa1 <- xy[1,1]-(dist.buff+10)
          ya1 <- xy[1,2]
          xa2 <- xy[1,1]+(dist.buff+10)
          ya2 <- xy[1,2]
        }
      }
    }

    di1 <- sqrt((xa1-xy[n,1])^2 +(ya1-xy[n,2])^2)
    di2 <- sqrt((xa2-xy[n,1])^2 +(ya2-xy[n,2])^2)

    if (di1>di2) {
      ln1 <- data.frame(x=c(xa1, xy[1,1]), y = c(ya1, xy[1,2]))
    } else ln1 <- data.frame(x=c(xa2, xy[1,1]), y = c(ya2, xy[1,2]))

    cl1a <- sp::Line(ln1)
    cl1b <- sp::Lines(list(cl1a), ID = 1)
    row.names(ln1) <- seq(1, dim(ln1)[1])
    shape.l1 <- sp::SpatialLines(list(cl1b), proj4string = sp::CRS(crs1))
    ln1 <- as.data.frame(ln1)
    ln1 <- sp::SpatialLinesDataFrame(shape.l1, ln1)

    ln1 <- sp::spTransform(x = ln1, CRSobj=crs1)
    comb1 <-  utils::combn(1:length(shape.ln),1)
    shape.ln <- apply(comb1, 2, function(i) sp::spTransform(x = shape.ln[[i]], CRSobj=crs1))

    p1a <- apply(comb1, 2, function(x) rgeos::gIntersection(shape.ln[[x]], ln1))

    id1.out<- which(unlist(apply(comb1, 2, function(x) length(p1a[[x]])))==1)

    shape.ln1 <- shape.ln[-id1.out]

    if (exists("shape.ln1")) shape.ln <- shape.ln1 else shape.ln1 <- shape.ln

    n2 <- length(shape.ln)
    comb2 <- utils::combn(1:n2, 1)

    if (xy[(n-1),1]-xy[n,1]== 0) m <- 0 else  m <- (xy[(n-1),2]-xy[n,2])/(xy[(n-1),1]-xy[n,1])

    if (m!=0){
      xa1 <- xy[n,1]-(dist.buff+10)
      b <- xy[n,2]-(m*xy[n,1])
      ya1 <- m * xa1 +b

      xa2 <- xy[n,1]+(dist.buff+10)
      ya2 <- m * xa2 +b

    } else {
      if (xy[(n-1),1]-xy[n,1]== 0 & m==0){
        xa1 <- xy[n,1]
        ya1 <- xy[n,2]-(dist.buff+10)

        xa2 <- xy[n,1]
        ya2 <- xy[n,2]+(dist.buff+10)
      } else {
        if(xy[(n-1),2]-xy[n,2]==0 & m==0){
          xa1 <- xy[n,1]-(dist.buff+10)
          ya1 <- xy[n,2]
          xa2 <- xy[n,1]+(dist.buff+10)
          ya2 <- xy[n,2]
        }
      }
    }

    di1 <- sqrt((xa1-xy[1,1])^2 +(ya1-xy[1,2])^2)
    di2 <- sqrt((xa2-xy[1,1])^2 +(ya2-xy[1,2])^2)

    if (di1>di2) {
      ln1 <- data.frame(x=c(xa1, xy[n,1]), y = c(ya1, xy[n,2]))
    } else ln1 <- data.frame(x=c(xa2, xy[n,1]), y = c(ya2, xy[n,2]))

    cl1a <- sp::Line(ln1)
    cl1b <- sp::Lines(list(cl1a), ID = 1)
    row.names(ln1) <- seq(1, dim(ln1)[1])
    shape.l1 <- sp::SpatialLines(list(cl1b), proj4string = sp::CRS(crs1))
    ln1 <- as.data.frame(ln1)
    ln1 <- sp::SpatialLinesDataFrame(shape.l1, ln1)

    ln1 <- sp::spTransform(x = ln1, CRSobj=crs1)
    shape.ln <- apply(comb2, 2, function(i) sp::spTransform(x = shape.ln[[i]], CRSobj=crs1))

    p1b <- apply(comb2, 2, function(x) rgeos::gIntersection(shape.ln[[x]], ln1))

    id2.out<- which(unlist(apply(comb2, 2, function(x) length(p1b[[x]])))==1)

    shape.ln1 <- shape.ln[-id2.out]

    if (exists("shape.ln1")) shape.ln <- shape.ln1 else shape.ln1 <- shape.ln

  } else {

    xy.buffln <- sp::coordinates(buff.ln)[[1]][[1]]
    xy.buffln2 <- sp::coordinates(buff.ln)[[1]][[2]]
    colnames(xy.buffln) <- c("x", "y")
    xybuff <- as.data.frame(xy.buffln)
    colnames(xy.buffln2) <- c("x", "y")
    xybuff2 <- as.data.frame(xy.buffln2)

    # make them line shapefiles and put them in a list result

    xy <- cbind(xybuff[,1], xybuff[,2])
    cl <- sp::Line(xy)
    cl1 <- sp::Lines(list(cl), ID = 1)
    row.names(xybuff) = seq(1, dim(xybuff)[1])

    shape.l <- sp::SpatialLines(list(cl1), proj4string = sp::CRS(crs1))
    shape.buff <- sp::SpatialLinesDataFrame(shape.l, xybuff)

    xy <- cbind(xybuff2[,1], xybuff2[,2])
    cl <- sp::Line(xy)
    cl1 <- sp::Lines(list(cl), ID = 1)
    row.names(xybuff2) = seq(1, dim(xybuff2)[1])

    shape.l <- sp::SpatialLines(list(cl1), proj4string = sp::CRS(crs1))
    shape.buff2 <- sp::SpatialLinesDataFrame(shape.l, xybuff2)

    shape.ln1 <- list()
    shape.ln1[[1]] <- shape.buff
    shape.ln1[[2]] <- shape.buff2
  }

  if (is.na(dir.name)){
    dir.name <- NA
  } else {

    dir.name <- gsub("\\\\", "/", dir.name)
    dir.create(dir.name, showWarnings = FALSE)
    dir.create(paste(dir.name, "/","Margins", sep = ""), showWarnings = FALSE)
    dir.mar <- paste(dir.name, "/","Margins", sep = "")
    comb1 <-  utils::combn(1:length(shape.ln1),1)

    apply(comb1, 2, function(x)
      raster::shapefile(shape.ln1[[x]], file = paste(dir.mar, "/mar_a",x, "_ID",ID1,"_",
                                                     dist.buff, "m.shp", sep="")))
  }
  return(shape.ln1)
}
