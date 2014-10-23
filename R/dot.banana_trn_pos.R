#'@export
.banana_trn_pos <- function(x, n_trn_pos, seed=NULL) {
  if (!is.null(seed))
    set.seed(seed)
  cells <- sample(which(banana$y[]==1), n_trn_pos)
  
  df <- extract(x=x$x, y=cells)
  rownames(df) <- cells
  return(df)
}

