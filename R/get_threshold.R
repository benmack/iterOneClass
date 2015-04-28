#' @export
get_threshold <- function(base_dir, iter) {
  
  if (verbose)
    cat("LOADING: ", iocc_filename(base_dir, iter, 
                                   "_results.RData"), "\n")
  load(iocc_filename(base_dir, iter, "_results.RData"))
  return(th)
}