#'@export
get_banana <- function() {

  fname <- system.file("extdata/banana_x.tif",
                       package="iterOneClass")
  if (fname == "") {
    fname = "inst/extdata/banana_x.tif"
  } 
  x <- brick(fname) 

  fname <- system.file("extdata/banana_y.tif",
                       package="iterOneClass")
  if (fname == "") {
    fname = "inst/extdata/banana_y.tif"
  } 
  y <- raster(fname) 

  return(list(x=x, y=y))
}
