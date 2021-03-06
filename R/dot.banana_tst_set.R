#' @export
.banana_tst_set <- function(x, n=NULL, n_pos=NULL, n_neg=NULL, seed=NULL) {
  
  y <- banana$y[]
  if (!is.null(n)) {
    if (!is.null(n_pos) | !is.null(n_neg))
      message("If \'n\' is given \'n_pos\' and \'n_neg\' are ignored.")
    if (!is.null(seed))
      set.seed(seed)
    cells <- sample(length(y), n)
  } else if (!is.null(n_pos) & !is.null(n_neg)) {
    if (!is.null(seed))
      set.seed(seed)
    cells_pos <- sample(which(y==1), n_pos)
    if (!is.null(seed))
      set.seed(seed)
    cells_neg <- sample(which(y==-1), n_neg)
    cells <- c(cells_pos, cells_neg)
  } else {
    error("Specify \'n\' or \'n_pos\' and \'n_neg\'.")
  }
  
  labels <- y[cells]
  values <- extract(x=x$x, y=cells)
  
  df <- data.frame(y=labels, values)
  rownames(df) <- cells
  
  return(df)
  
}
