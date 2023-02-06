#' Secondary inflections on bluff/cliff face
#'
#' The function looks for the major "positive" and "negative" inflections on the face
#' of the bluff/cliff profile between top and toe.
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
#'
#' @param toe1 - A list of bluff/cliff toe data.frame with one row and same column
#' names as the data.frame from profs list. This is the results from get.toptoe function.
#'
#' @param iHi - Is a threshold in meters that will classify top2 and toe2 as
#' significant if their detrended elevation is equal or greater than threshold.
#'
#' @return A list of lists with two elements. The first element is a list of secondary tops data.frame (top2).
#' The second elemnt is a list of secondary toes data.frame (toe2). The data.frames have the following columns:
#' xi, yi, zi, id, id1, ID.prof, len, v8, signif1 where:
#' - xi, yi, zi - coordinates, where zi represents elevation (in meters);
#' - id - is a sequential id of respective secondary top/toe row in the original profile table;
#' - id1 = is the cumulative distance between points on profile that corresponds to the secondary top/toe;
#' - ID.prof = is an index of the profile, if the list of profile tables has more than
#' one table then this index will identify uniquely the profile.
#' - len represents the detrended value of the elevation for secondary top/toe;
#' - v8 is the sequential id of respective secondary top/toe row in the bluff/cliff face profile table;
#' - signif1 is 0 for not significant and 1 for significant.
#'
#' @export
#'
#' @examples
#' # Do not run
#' # Use the prof.noveg list of data.frames and the bluff / cliff top/toe list (top1/toe1) for
#' # obtaining the bluff/cliff face profile. See get.toptoe function example.
#' #
#' # n10 <- length(prof.noveg)
#' # comb.n10 <- combn(1:n10,1)
#' #
#' # sec.toptoe <- apply(comb.n10, 2, function(x)
#' #   iBSecond(prof.noveg[[x]], top1[[x]], toe1[[x]], iHi=1))
#' #
#' # n11 <- length(sec.toptoe)
#' # comb.n11 <- combn(1:n11,1)
#' #
#' # top2 <- apply(comb.n11, 2, function(x) sec.toptoe[[x]][[1]])
#' #
#' # toe2 <- apply(comb.n11, 2, function(x) sec.toptoe[[x]][[2]])
#' #
iBSecond <- function(profs, top1, toe1, iHi){

  n <- length(profs)
  comb1 <-  utils::combn(1:n,1)

  try(if(is.list(profs)& is.data.frame(profs)==TRUE) stop ("profile cannot be a data.frame"))
  dist1 <- profs[[1]][2,5]-profs[[1]][1,5]

  # get secondary inflections of the face of the bluff

  profs1 <- apply(comb1, 2, function(x)
    tt.prof(profs[[x]], top1, toe1))

  profs1 <- profs1[!is.na(profs1)]

  profs1a <- profs1

  n <- length(profs1)
  comb1 <-  utils::combn(1:n,1)

  len1 <- bLen(profs1)

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  profs1 <- apply(comb1, 2, function(x)
    if(is.list(len1)) data.frame(profs1[[x]], len= len1[[x]])
    else data.frame(profs1[[x]], len= len1))

  p2 <- which(p1>5)

  profs1 <- profs1[p2]

  # this is repeated in case any profiles were eliminated
  n <- length(profs1)
  comb1 <-  utils::combn(1:n,1)

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])


  profs1 <- apply(comb1, 2, function(x) if(min(profs1[[x]]$len)[1]< 0 )
    profs1[[x]][c(-1, -p1[x]),] else profs1[[x]])

  profs1 <- apply(comb1, 2, function(x) data.frame(profs1[[x]], v8 =seq(1, dim(profs1[[x]])[1])))

  bot2 <- apply(comb1, 2, function(x) if(max(profs1[[x]]$len)!=min(profs1[[x]]$len))
    profs1[[x]][profs1[[x]]$len==min(profs1[[x]]$len),][1,]
    else matrix(rep(NA, 8), nrow=1, ncol=8, byrow=TRUE))

  bot2.sig <- apply(comb1, 2, function(x)
    if(is.data.frame(bot2[[x]]))
      if(abs(bot2[[x]]$len)>= iHi) 1 else 0
    else NA)

  bot2 <- apply(comb1, 2, function(x) data.frame(bot2[[x]], signif1=bot2.sig[[x]]))

  profs1 <- profs1a

  #what if the whole profile is concave and max(len)=0?

  if(is.list(len1)){

    rez1<- apply(comb1, 2, function(x) if(length(len1[[x]]>3)) {

      rez1 <- plus.len(profs1[x], len1[[x]])

    } else rez1 <- list(len1=len1[[x]], profs1=profs1[[x]]))

    len1 <- apply(comb1, 2, function(x) rez1[[x]][[1]])

    profs1 <- apply(comb1, 2, function(x) rez1[[x]][[2]])

  } else {

    rez1 <- plus.len(profs, len1)
    len1<- rez1[[1]]

    profs1<- rez1[2]
  }

  profs1 <- apply(comb1, 2, function(x)
    if(is.list(len1)) data.frame(profs1[[x]], len1= len1[[x]])
    else data.frame(profs1[[x]], len1= len1))

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  p2 <- which(p1>5)
  profs1 <- profs1[p2]

  # this is repeated in case any profiles were eliminated
  n <- length(profs1)
  comb1 <-  utils::combn(1:n,1)

  p1 <- apply(comb1, 2, function(x) dim(profs1[[x]])[1])

  profs1 <- apply(comb1, 2, function(x) profs1[[x]][c(-1, -p1[x]),])

  profs1 <- apply(comb1, 2, function(x) data.frame(profs1[[x]], v8 =seq(1, dim(profs1[[x]])[1])))

  cname <- names(profs1[[1]])

  top2 <- apply(comb1, 2, function(x) if(max(profs1[[x]]$len1)!=min(profs1[[x]]$len1))
    profs1[[x]][profs1[[x]]$len1==max(profs1[[x]]$len1),][1,]
    else matrix(rep(NA, 8), nrow=1, ncol=8, byrow=TRUE))


  top2.sig <- apply(comb1, 2, function(x)
    if(is.data.frame(top2[[x]]))
      if(abs(top2[[x]]$len1)>= iHi) 1 else 0
    else NA)

  top2 <- apply(comb1, 2, function(x) data.frame(top2[[x]], signif1=top2.sig[[x]]))

  rez <- list(sec.top = top2, sec.toe = bot2)

  return (rez)
}
