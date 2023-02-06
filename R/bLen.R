# len helper function - detrending the profile
#
# profs is a list of profiles with xyz coordinates, id and id1 and ID.prof
# in this order
#
# id1 actually represents the cumulative distance between the points dist1
# far apart

bLen <- function(profs){

  try(if(is.list(profs)& is.data.frame(profs)==TRUE) stop ("profile cannot be a data.frame"))

  n <- length(profs)
  comb1 <-  utils::combn(1:n,1)

  p <- apply(comb1, 2, function(x) dim(profs[[x]])[1])

  m.l <- apply(comb1, 2, function(x) if(p[x]>3)
    (profs[[x]][1,3]-profs[[x]][p[x],3]) / (profs[[x]][1,5]-profs[[x]][p[x],5])
    else NA)

  m.l <- apply(comb1, 2, function(x) 	if(!is.na(m.l[x]) & m.l[x] ==0)
    m.l[x] <- 2.6e-16 else m.l[x] <- m.l[x])

  b.l <- apply(comb1, 2, function(x) if(!is.na(m.l[x]))
    profs[[x]][1,3] -m.l[x] *profs[[x]][1,5]
    else NA)

  yi <- apply(comb1, 2, function(x) if(!is.na(m.l[x]))
    m.l[x]*profs[[x]][,5]+b.l[x]
    else NA)

  a.m <- apply(comb1, 2, function(x) if(!is.na(m.l[x]))
    atan(m.l[x])else NA)

  len1 <- apply(comb1, 2, function(x)
    if(is.list(yi))(profs[[x]][,3]-yi[[x]])*cos(a.m[x])
    else (profs[[x]][,3]-yi)*cos(a.m[x]))

  return(len1)

}
