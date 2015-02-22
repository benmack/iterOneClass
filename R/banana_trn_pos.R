#'@export
banana_trn_pos <- function(n_trn_pos, seed=123) {
  
  xy <- get_banana() 
  
  if (!is.null(seed))
    set.seed(seed)
  cells <- sample(which(xy$y[]==1), n_trn_pos)
  
  df <- extract(x=xy$x, y=cells)
  rownames(df) <- cells
  return(df)
}

