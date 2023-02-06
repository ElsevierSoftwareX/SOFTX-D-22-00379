#' Get top and toe coordinates on an elevation bluff / cliff profile
#'
#' The function first determines the position of the toe on the bluff profile in
#' case of profile concavity, before it gets the top.
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
#' @param top.par - A TRUE/FALSE parameter to check that the top elevation
#' is above the mean range elevation and force the top to be above it. This is useful
#' in particular situations when the profile has a very exacerbated convex
#' portion that is not considered the top of the bluff.
#'
#' @param veg - A TRUE/FALSE parameter to eliminate 1 point anomalies on the profile if
#' vegetation was not totally eliminated from the DEM. The default value is FALSE.
#'
#'
#' @return A list of four lists of data.frames for each processed buffer.
#' First list within a buffer list has coordinates and ID for top of bluffs.
#' Second list within a buffer list has coordinates and ID for toe of bluffs.
#' Third list within a buffer list has the data used when parameter veg=TRUE.
#' Forth list within a buffer has the data when parameter veg=FALSE.
#' @export
#'
#' @examples
#' # Do not run
#' # profs is the list of lists of data.fame of profiles.
#' # See the example for the bProf function.
#' #
#' # n4 <- length(profs)
#' # comb.n4 <- combn(1:n4,1)
#' #
#' # toptoe1 <- apply(comb.n4, 2, function(x)
#' #   get.toptoe(profs[[x]], top.par=TRUE, veg=TRUE))
#' #
#' # n5 <- length(toptoe1)
#' # comb.n5 <- combn(1:n5, 1)
#' #
#' # list of bluff tops coordinates
#' # top1 <- apply(comb.n5, 2, function(x) toptoe1[[x]][[1]])
#' #
#' # list of bluff toes coordinates
#' # toe1 <- apply(comb.n5, 2, function(x) toptoe1[[x]][[2]])
#' #
#' # list of profiles with small errors removed
#' # prof.noveg <- apply(comb.n5, 2, function(x) toptoe1[[x]][[3]])
#' #
#' # list of original profiles
#' # prof.orig <- apply(comb.n5, 2, function(x) toptoe1[[x]][[4]])
#' #
get.toptoe <- function(profs, top.par = TRUE, veg = FALSE) {

  try(if(is.list(profs)& is.data.frame(profs)==TRUE) stop ("profile cannot be a data.frame"))

  n <- length(profs)
  comb1 <-  utils::combn(1:n,1)

  p <- apply(comb1, 2, function(x) dim(profs[[x]])[1])
  p1 <- which(p>5)

  #eliminate profiles with only 5 values or less.
  profs <- profs[p1]
  n <- length(profs)

  comb1 <-  utils::combn(1:n,1)
  p <- apply(comb1, 2, function(x) dim(profs[[x]])[1])
  prof.orig <- profs

  # veg = TRUE
  #try to eliminate potential vegetation or small errors (1 point) in DEM, and any flat water

  if (veg==TRUE) {

    n <- length(profs)
    comb1 <-  utils:: combn(1:n,1)

    profs <- apply(comb1, 2, function(x) profs[[x]] <- data.frame(profs[[x]],
                                                                  diff1 = c(diff(profs[[x]][,3]),
                                                                            diff(profs[[x]][,3])[length(diff(profs[[x]][,3]))])))

    p <- apply(comb1, 2, function(x) dim(profs[[x]])[1])

    profsa <- apply(comb1, 2, function(x) profs[[x]][profs[[x]]$diff1>=0,])

    profsa <- apply(comb1, 2, function(x) if (profsa[[x]][1,4]!=1)
      profsa[[x]] <- data.frame(rbind(profsa[[x]][1,], profsa[[x]]))
      else profsa[[x]]<- profsa[[x]])

    profsa <- apply(comb1, 2, function(x) if (profsa[[x]][dim(profsa[[x]])[1],4]!=profs[[x]][dim(profs[[x]])[1],4])
      profsa[[x]] <- data.frame(rbind(profsa[[x]], profsa[[x]][dim(profsa[[x]])[1],]))
      else profsa[[x]]<- profsa[[x]])

    for (i in 1:n){
      profsa[[i]][1, c(4,5)] <- profs[[i]][1, c(4,5)]
      profsa[[i]][dim(profsa[[i]])[1], c(4,5)] <- profs[[i]][dim(profs[[i]])[1], c(4,5)]
    }
    profs <- profsa
  }

  # calculate the toe of the bluff first

  profs <- apply(comb1, 2, function(x) profs[[x]][,-7])

  n <- length(profs)
  comb1 <-  utils::combn(1:n,1)

  p <- apply(comb1, 2, function(x) dim(profs[[x]])[1])

  len1 <- bLen(profs)

  profs <- apply(comb1, 2, function(x)
    if(is.list(len1)) data.frame(profs[[x]], len=len1[[x]])
    else data.frame(profs[[x]], len=len1))

  prof <- profs

  profs <- apply(comb1, 2, function(x) if(min(profs[[x]]$len)[1]< 0 )
    profs[[x]][c(-1, -p[x]),] else profs[[x]])

  profs <- apply(comb1, 2, function(x) data.frame(profs[[x]], v8 =seq(1, dim(profs[[x]])[1])))

  bot <- apply(comb1, 2, function(x) if(max(profs[[x]]$len)!=min(profs[[x]]$len))
    profs[[x]][profs[[x]]$len==min(profs[[x]]$len),][1,]
    else base::as.data.frame(matrix(rep(NA, 8), nrow=1, ncol=8, byrow=TRUE)))

  cbot <- names(profs[[1]])

  bot<- lapply(bot, stats::setNames, cbot)

  id.nna <- unlist(apply(comb1, 2, function(x)
    if(!is.na(bot[[x]][[1]])) x))

  bot <- bot[id.nna]

  profs <- profs[id.nna]

  n.1 <- length(bot)
  comb.1 <- utils::combn(1:n.1,1)

  m.t <- apply(comb.1, 2, function(x) round(mean(range(profs[[x]][,8]))))

  toe.v8 <- apply(comb.1, 2, function(x) bot[[x]][[8]])

  for (x in 1:n.1){

    if(m.t[[x]] < toe.v8[[x]]) {
      while (m.t[[x]] < toe.v8[[x]] & dim(profs[[x]])[1] > 5)
      {profs[[x]] <- profs[[x]][-(toe.v8[[x]]:max(profs[[x]][,8])),];
      profs[[x]][,8] <- seq(1, dim(profs[[x]])[1]);
      bot[[x]]<- profs[[x]][profs[[x]]$len==min(profs[[x]]$len),][1,];
      toe.v8[[x]] <- bot[[x]][[8]];
      m.t[[x]] <- round(mean(range(profs[[x]][,8])))}
    } else bot[[x]]<-bot[[x]]
  }

  bot <- apply(comb1, 2, function(x) bot[[x]][1:6])

  # get the top of the cliff

  profs <- apply(comb1, 2, function(x) prof[[x]][,-7])

  profs <- profs[id.nna]

  n <- length(profs)
  comb1 <- utils::combn(1:n,1)

  profs1 <- apply(comb1, 2, function(x) profs[[x]][profs[[x]][,5]>= bot[[x]][[5]],])

  p <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  p1 <- which(p>5)

  #eliminate profiles with only 5 values or less.
  profs1 <- profs1[p1]
  n <- length(profs1)
  comb1 <-  utils::combn(1:n,1)

  profs1 <- apply(comb1, 2, function(x) data.frame(profs1[[x]], v7 =seq(1, dim(profs1[[x]])[1])))

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  len1a <- bLen(profs1)

  #what if the whole profile is concave and max(len)=0?

  if(is.list(len1a)){

    rez1<- apply(comb1, 2, function(x) if(length(len1a[[x]]>3)) {
      rez1 <- plus.len(profs1[x], len1a[[x]])
    } else rez1 <- list(len1a=len1a[[x]], profs1=profs1[[x]]))

    len1a <- apply(comb1, 2, function(x) rez1[[x]][[1]])

    profs1 <- apply(comb1, 2, function(x) rez1[[x]][[2]])

  } else {

    rez1 <- plus.len(profs1, len1a)
    len1a<- rez1[[1]]

    profs1<- rez1[2]
  }

  profs1 <- apply(comb1, 2, function(x)
    if(is.list(len1a)) data.frame(profs1[[x]], len1= len1a[[x]])
    else data.frame(profs1[[x]], len1= len1a))

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  profs1 <- apply(comb1, 2, function(x) profs1[[x]][c(-1, -p1[x]),])

  top <- apply(comb1, 2, function(x) if(max(profs1[[x]]$len1)!=min(profs1[[x]]$len1))
    profs1[[x]][profs1[[x]]$len1==max(profs1[[x]]$len1),][1,]
    else base::as.data.frame(matrix(rep(NA, 8), nrow=1, ncol=8, byrow=TRUE)))

  ctop <- names(profs1[[1]])
  top<- lapply(top, stats::setNames, ctop)

  id.nna <- unlist(apply(comb1, 2, function(x)
    if(!is.na(top[[x]][[1]])) id.nna[[x]]<- x))

  top <- top[id.nna]
  profs1 <- profs1[id.nna]

  n1 <- length(top)
  comb1 <- utils::combn(1:n1,1)

  if (top.par ==TRUE){

    m <- apply(comb1, 2, function(x) mean(range(profs1[[x]][,3])))

    top.el <- apply(comb1, 2, function(x) top[[x]][3])

    for (x in 1:n1){
      if(top.el[[x]] < m[[x]]) {
        while (top.el[[x]] < m[[x]] & dim(profs1[[x]])[1] > 3)
        {profs1[[x]] <- profs1[[x]][-(1:top[[x]][[7]]),];
        profs1[[x]][,7] <- seq(1, dim(profs1[[x]])[1]);
        top[[x]] <- profs1[[x]][profs1[[x]]$len1==max(profs1[[x]]$len1),][1,];
        top.el[[x]] <- top[[x]][[3]];
        m[[x]] <- mean(range(profs1[[x]][,3]))}
      } else top[[x]]
    }

  } else top <- top

  top <- apply(comb1, 2, function(x) top[[x]][1:6])

  n2 <- length(prof)
  combn2 <- utils::combn(1:n2, 1)
  prof <- apply(combn2, 2, function(x) prof[[x]][1:6])

  n3 <- length(prof.orig)
  combn3 <- utils::combn(1:n3, 1)
  prof.orig <- apply(combn3, 2, function(x) prof.orig[[x]][1:6])

  out <- list(top1 = top, bot1 = bot, prof.remveg = prof, prof1.orig = prof.orig)
  return(out)
}
