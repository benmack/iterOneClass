#'@export
load_iterativeOcc <- function (baseDir, iter, u=NULL) {

  load(get_fname("ini", baseDir))
  load(get_fname("results", baseDir, iter))
  
  objs <- ls()
  
  rtrn <- list()
  for ( i in 1:length(objs) ) {
    rtrn[[i]] <- eval(parse(text=objs[i]))
  }
  names(rtrn) <- objs
  
  return(rtrn)
}