#'@export
get_finIter <- function(baseDir)
  return(length(Sys.glob(paste0(baseDir, "/iter-*_model.RData"))))
