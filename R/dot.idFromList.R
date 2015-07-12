#'@export
.idFromList <- function(lst, what)
  return(sapply(strsplit(names(lst), what),
                function(x) as.numeric(x[2])))
