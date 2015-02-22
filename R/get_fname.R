#' @name get_fname
#' @title Get the filename of saved data 
#' @description \code{\link{iterativeOcc}} and other functions store intermediate and final results to disc. With this function the filename of specific data packages can be generated. 
#' @details The following values are possible:
#' \itemize{
#' \item ini
#' \item ...
#' @param what Filename of what kind of data.
#' @param 
#' @export
get_fname <- function(what, base_dir, iter=NULL, modRow=NULL) {

  if (what == "ini") {
    return(iocc_filename(base_dir, 0, "_ini.RData"))
  } 
}
