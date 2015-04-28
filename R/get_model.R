#' @export
get_model <- function(base_dir, iter, modRow=NULL,
                      verbose=FALSE) {
  
  if (verbose)
    cat("LOADING: ", iocc_filename(base_dir, iter, 
                                   "_results.RData"), "\n")
  load(get_fname("model", base_dir, iter=iter))
  
  if (!is.null(modRow)) {
    if (modelPosition(model)$row != modRow) {
      if (verbose)
        cat("UPDATING MODEL TO MODEL #", modRow, "\n")
      model <- update(model, modRow=modRow)
      
    }
  }
  # model$th <- th
  return(model)
}