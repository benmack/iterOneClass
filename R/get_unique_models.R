#' @export
get_unique_model <- function(baseDir, iters, model=NULL, subfolder=NULL) {
  
  modRows_unique <- list()
  counter <- 0
  for (i in iters) {
    counter <- counter + 1
    fname <- get_fname("unique_models", baseDir, iter=i, subfolder=subfolder)
    if (!file.exists(fname)) {
      hops <- get_puSet(baseDir, i, model=model)
      hops_df <- as.data.frame(t(sapply(hops, function(x) c(x$pos, x$un))))
      modRows_unique_i <- which(!duplicated(hops_df))
      save(modRows_unique_i, file=fname)
      n_all <- length(hops)
    } else { 
      load(fname)
      n_all <- "?"
    }
    modRows_unique[[counter]] <- modRows_unique_i
    cat("Iteration", i, ":", length(modRows_unique[[counter]]), 
        "/", n_all, "\n")
  }
  return(modRows_unique)
}
