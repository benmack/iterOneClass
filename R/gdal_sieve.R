gdal_sieve <- function(srcfile = srcfile,
                       dstfile = dstfile,
                       st = 25,
                       q=TRUE, returnRaster=FALSE) {
  require(gdalUtils)

  executable <- "gdal_sieve.bat"
  parameter_variables <- list(
    logical = list(
      varnames <- c("q", "4", "8", "nomask")),
    vector = list(
      varnames <- c("st")),
    scalar = list(
      varnames <- c()),
    character = list(
      varnames <- c("of","mask")),
    repeatable = list(
      varnames <- c()))
  parameter_order <- c(
    "q", "st", "4", "8", "srcfile", "nomask", "mask", "of", "dstfile")
  parameter_noflags <- c("srcfile", "dstfile")
  parameter_logical <- c("q")
  # Now assign some parameters:
  parameter_values = list(
    srcfile = srcfile,
    dstfile = dstfile,
    st = st,
    q=TRUE)
  
  cmd <- gdal_cmd_builder(executable=executable,
                          parameter_variables=parameter_variables,
                          parameter_values=parameter_values,
                          parameter_order=parameter_order,
                          parameter_noflags=parameter_noflags)
  
  # cat(cmd)
  system(cmd,intern=TRUE)
  if (returnRaster)
    return(raster(dstfile))
}