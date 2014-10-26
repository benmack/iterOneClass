load_iterativeOcc <- function ( folder_out, iter, 
                                       u=NULL) {

  load( .fname(folder_out, 0, ".RData", "initialized") )
  load(.fname(folder_out, iter, ".RData", "results") )
  
  objs <- ls()
  
  rtrn <- list()
  for ( i in 1:length(objs) ) {
    rtrn[[i]] <- eval(parse(text=objs[i]))
  }
  names(rtrn) <- objs
  
  return(rtrn)
}