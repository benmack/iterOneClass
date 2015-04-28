#' @export
.nIter <- function(baseDir)
  return(length(grep("results.RData", dir(baseDir))))
