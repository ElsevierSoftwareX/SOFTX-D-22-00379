# Function that replaces outliers with NA in a table of x and y coordinates
# outliers are identified using box-and-whisker plots on both x and y coordinates
#outliers are considered values that are either less than Q1 - 1.5*IQR,
# or greater than Q3 + 1.5 IQR, where:
# Q1 is the first quartile (25%)
# Q3 is the third quatile (75%).
#IQR is the inter-quartile range, IQR = Q3 - Q1

out1 <- function(xy.1){

  if (is.null(dim(xy.1)[1])) {
    xy.1 <- xy.1
  } else {
    xy.1 <- as.matrix(xy.1)

    rownames(xy.1) <- as.character(seq(1, dim(xy.1)[1]))

    if (dim(xy.1)[1] > 3) {

      a<-graphics::boxplot(xy.1[,1], plot=FALSE)
      b <- graphics::boxplot(xy.1[,2], plot=FALSE)
      idx <- as.numeric(names(a$out))
      idy <- as.numeric(names(b$out))
      id <- unique(c(idx, idy))

      if (length(id)!= 0) xy.1[id,] <- rep(NA, dim(xy.1)[2]) else xy.1 <- xy.1
    } else {
      xy.1 <- xy.1
    }
  }
  xy.1 <- as.data.frame(xy.1)
  xy.1 <- xy.1[!is.na(xy.1[,1]),]
  return(xy.1)
}
