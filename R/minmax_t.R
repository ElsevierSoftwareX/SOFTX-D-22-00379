###min.t and max.t helper functions for function get.minmax

## tabl1 - a data.frame with xi, yi, zi, id, id1, ID.prof and len1 columns
## in this order and with these names
## t.ids <- numerical vector of ids


min.t <- function(tabl1, t.ids){

  k2 <-length(t.ids)

  combk2 <- utils::combn(1:k2, 1)

  tab1 <- list()
  cnames <- c("xi", "yi", "zi", "id", "id1", "ID.prof", "len1")

  for (i in 1:k2){
    if(t.ids[i]!=0){
      tab1[[i]] <- tabl1[tabl1$id <= t.ids[i],]
      tabl1 <- tabl1[tabl1$id > t.ids[i],]
    } else {
      tab1[[i]] <- tabl1
      tabl1 <- tabl1
    }
  }

  min1 <- apply(combk2, 2, function(x) tab1[[x]][tab1[[x]]$len1==min(tab1[[x]]$len1),])

  if(length(min1)>1){
    min1 <- apply(combk2, 2, function(x) base::as.data.frame(min1[[x]]))

    min1 <- data.table::rbindlist(min1)

    min1 <- as.matrix(min1)

    min1 <- base::as.data.frame(min1[!is.na(min1[,1]),])

    colnames(min1) <- cnames
  } else {
    min1 <- min1
  }

  return(min1)

}

#################################################################################

max.t <- function(tabl1, t.ids){

  k2 <-length(t.ids)

  combk2 <- utils::combn(1:k2, 1)

  tab1 <- list()
  cnames <- c("xi", "yi", "zi", "id", "id1", "ID.prof", "len1")

  for (i in 1:k2){
    if(t.ids[i]!=0){
      tab1[[i]] <- tabl1[tabl1$id <= t.ids[i],]
      tabl1 <- tabl1[tabl1$id > t.ids[i],]
    } else {
      tab1[[i]] <- tabl1
      tabl1 <- tabl1
    }
  }

  max1 <- apply(combk2, 2, function(x) tab1[[x]][tab1[[x]]$len1==max(tab1[[x]]$len1),])

  if(length(max1)>1){
    max1 <- apply(combk2, 2, function(x) base::as.data.frame(max1[[x]]))

    max1 <- data.table::rbindlist(max1)

    max1 <- as.matrix(max1)

    max1 <- base::as.data.frame(max1[!is.na(max1[,1]),])

    colnames(max1) <- cnames
  } else {
    max1 <- max1
  }

  return(max1)
}

