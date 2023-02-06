### read a shape file when you have a full path including the file name and
### shp extension and how to parse the dns and layer params for readOGR function

### e.g. shape.path = "my_directory/my_shapefile.shp"
### it is just a wrapper for readOGR

shape.read<- function(shape.path){

  shape.path <- gsub("\\\\", "/", shape.path)
  p1 <- stringr::str_split(shape.path, "/")
  n1 <- length(p1[[1]])

  fn<- stringr::str_split(p1[[1]][n1],"\\.")

  layer <- fn[[1]][1]

  p3 <- p1[[1]][-n1]

  dsn <- paste0(p3, collapse = "/")

  shape.file <- rgdal::readOGR(dsn, layer, integer64="allow.loss")

  return(shape.file)
}
