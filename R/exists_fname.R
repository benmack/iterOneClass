#' @export
exists_fname <- function(what, base_dir, iter=NULL, modRow=NULL) {
  
  fname <- get_fname(what, basdir, iter=iter, modRow=modRow)
  
  return(file.exists(fname))

}
