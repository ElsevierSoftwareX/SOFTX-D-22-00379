## The function limits the profile between top and toe to be analysed by the
# convex.concave function iRastCC.
## prof1 is only 1 profile table with the following columns in this strict order:
## "xi"      "yi"      "zi"      "id"      "id1"     "ID.prof"
## top1 is a list of 1 row data.frame with same columns as the prof1 that
## correspond to the top of the bluff in all existing profiles within a buffer DEM
## toe1 is a list of 1 row data.frame with same columns as the prof1 that
## correspond to the toe of the bluff in all existing profiles within a buffer DEM
## in prof1, top1, and toe1 the ID.prof which is the 6th column always
## corresponds to the profile ID for the top or the toe, or the profile itself.

profCC <- function(prof1, top1, toe1){

  tab.top <- base::as.data.frame(data.table::rbindlist(top1))

  tab.toe <- base::as.data.frame(data.table::rbindlist(toe1))

  n.top <- tab.top[,6]
  n.toe <- tab.toe[,6]
  id.prof1 <- prof1[1,6]

  id.top <- which(n.top==id.prof1)
  id.toe <- which(n.toe==id.prof1)

  if(length(id.top)==1 & length(id.toe)==1) {

    profs1 <- prof1[prof1[,4]>=tab.toe[id.toe,4] & prof1[,4]<= tab.top[id.top,4],]

  } else if (length(id.top)==1 & length(id.toe)==0) {
    profs1 <- prof1[prof1[,4]<= tab.top[id.top,4],]
  } else if (length(id.top)==0 & length(id.toe)==1) {
    profs1 <- prof1[prof1[,4]>=tab.toe[id.toe,4],]
  } else {

    profs1 <- prof1
  }

  return(profs1)
}
