#' @export
exists_fname <- function(what, base_dir, iter=NULL, modRow=NULL) {
  
  fname <- get_fname(what, base_dir, iter=iter, modRow=modRow)
  
  return(file.exists(fname))

}
