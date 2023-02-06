### helper functions get.mint and get.maxt, they are used in iDune function

### prof1b - a data.frame with xi, yi, zi, id, id1, ID.prof and len1 columns
### in this order and with these names
### len1 is the detrended elevation

### vals1 - a numeric vector that gives breaks in the elevation detrended values len1
### vals1 is calculated in iDune function


get.maxt <- function(prof1b, vals1){

  n.1 <- length(vals1)
  comb.1 <-  utils::combn(1:n.1,1)
  pr1 <- apply(comb.1, 2, function(x) prof1b[which(prof1b$len1>=vals1[x]),])
  p1 <- apply(comb.1, 2, function(x) dim(pr1[[x]])[1])
  diff1 <- apply(comb.1, 2, function(x) diff(pr1[[x]]$id))
  diff2 <- apply(comb.1, 2, function(x) which(diff1[[x]]>1))

  if(length(diff2)==0) diff2 <- rep(0, n.1)

  if(class(diff2)[1]=="integer"){
    diff2 <- as.list(diff2)
  } else {
    if (!is.list(diff2) & class(diff2)[1]=="matrix"){
      diff2 <- as.list(base::as.data.frame((diff2)))
    } else diff2<- diff2
  }

  ids <- apply(comb.1, 2, function(x)
    if(length(diff2[[x]])!=0)
      c(pr1[[x]]$id[diff2[[x]]], pr1[[x]]$id[dim(pr1[[x]])[1]])
    else 0)

  if(class(ids)[1]=="integer") ids<- t(as.matrix(ids)) else ids<- ids
  if(!is.list(ids))
    ids <- unlist(apply(comb.1, 2, function(x) list(ids[,x])), recursive=FALSE) else ids<- ids

  maxt <- apply(comb.1, 2, function(x) max.t(pr1[[x]], ids[[x]]))

  cnames1 <- c("xi", "yi", "zi", "id", "id1", "ID.prof", "len1")

  if (!is.list(maxt)) {

    n.2 <- dim(maxt)[2]
    comb.2 <-  utils::combn(1:n.2,1)

    maxt <- apply(comb.2, 2, function(x)
      base::as.data.frame(matrix(t(maxt[,1]), nrow=1, byrow=TRUE)))
    maxt<- lapply(maxt, stats::setNames, cnames1)
  } else maxt<- maxt

  maxt <- apply(comb.1, 2, function(x) base::as.data.frame(maxt[[x]]))

  maxt1 <- data.table::rbindlist(maxt)

  maxt1 <- as.matrix(maxt1)

  maxt1 <- base::as.data.frame(maxt1[!is.na(maxt1[,1]),])

  tr1 <- with(maxt1, table(id1))

  maxt1a <- unique(maxt1)

  maxt1a <- maxt1a[order(maxt1a$id1),]

  maxt1a$count <- tr1
  maxt1a <- as.matrix(maxt1a)

  return(maxt1a)
}

###############################################################

get.mint <- function(prof1b, vals1){

  n.1 <- length(vals1)
  comb.1 <-  utils::combn(1:n.1,1)
  pr1 <- apply(comb.1, 2, function(x) prof1b[which(prof1b$len1<=vals1[x]),])
  p1 <- apply(comb.1, 2, function(x) dim(pr1[[x]])[1])
  diff1 <- apply(comb.1, 2, function(x) diff(pr1[[x]]$id))
  diff2 <- apply(comb.1, 2, function(x) which(diff1[[x]]>1))

  if(length(diff2)==0) diff2 <- rep(0, n.1)

  if(class(diff2)[1]=="integer"){
    diff2 <- as.list(diff2)
  } else {
    if (!is.list(diff2) & class(diff2)[1]=="matrix"){
      diff2 <- as.list(base::as.data.frame((diff2)))
    } else diff2<- diff2
  }

  ids <- apply(comb.1, 2, function(x)
    if(length(diff2[[x]])!=0)
      c(pr1[[x]]$id[diff2[[x]]], pr1[[x]]$id[dim(pr1[[x]])[1]])
    else 0)

  if(class(ids)[1]=="integer") ids<- t(as.matrix(ids)) else ids<- ids

  if(!is.list(ids))
    ids <- unlist(apply(comb.1, 2, function(x) list(ids[,x])), recursive=FALSE) else ids<- ids

  mint <- apply(comb.1, 2, function(x) min.t(pr1[[x]], ids[[x]]))

  cnames1 <- c("xi", "yi", "zi", "id", "id1", "ID.prof", "len1")

  if (!is.list(mint)) {

    n.2 <- dim(mint)[2]
    comb.2 <-  utils::combn(1:n.2,1)

    mint <- apply(comb.2, 2, function(x)
      base::as.data.frame(matrix(t(mint[,1]), nrow=1, byrow=TRUE)))
    mint<- lapply(mint, stats::setNames, cnames1)
  } else mint<- mint

  mint <- apply(comb.1, 2, function(x) base::as.data.frame(mint[[x]]))

  mint1 <- data.table::rbindlist(mint)

  mint1 <- as.matrix(mint1)

  mint1 <- base::as.data.frame(mint1[!is.na(mint1[,1]),])

  tr1 <- with(mint1, table(id1))

  mint1a <- unique(mint1)

  mint1a <- mint1a[order(mint1a$id1),]

  mint1a$count <- tr1
  mint1a <- as.matrix(mint1a)

  return(mint1a)
}
