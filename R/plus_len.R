# helper function if all len values are negative - profile is concave
# the function eliminates one by one points from the profile and re-calculates
# the detrended elevation until at least one detrended elevation is positive.

plus.len <- function(prof1.1, len1.1){

  while (max(len1.1)<=0 & length(len1.1)>3)
  {p1 <- dim(prof1.1[[1]])[1];
  prof1.1[[1]] <- prof1.1[[1]][-p1,];
  p1 <- dim(prof1.1[[1]])[1];
  m.l1 <- (prof1.1[[1]][1,3]-prof1.1[[1]][p1,3])/(prof1.1[[1]][1,5]-prof1.1[[1]][p1,5]);
  if(m.l1 ==0) m.l1 <- 2.6e-16 else m.l1 <- m.l1;
  b.l1 <- prof1.1[[1]][1,3] -m.l1 *prof1.1[[1]][1,5];
  yi1 <- m.l1*prof1.1[[1]][,5]+b.l1;
  a.m1 <- atan(m.l1);
  len1.1<- (prof1.1[[1]][,3]-yi1)*cos(a.m1)}

  rez <- list(len1.1 = len1.1, prof1.1=prof1.1[[1]])
  return(rez)
}

