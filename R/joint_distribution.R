#' @export
joint_distribution <- function(df, xintercept=NULL,
                               yintercept=NULL, n=20000, 
                               seed=NULL, fname=NULL) {
  
  require(MASS)
  require(ggplot2)
  
  cn <- colnames(df)
  
  if (is.null(n)) {
    idx <- !logical(nrow(df))
  } else {
    if (!is.null(seed))
      set.seed(seed)
    idx <- sample(nrow(df), min(n, nrow(df)))
  }
  # dens <- kde2d(df[,1], df[,2])
  # densdf <- data.frame(expand.grid(dens$x, 
  #                                  dens$y),
  #                      z = as.vector(dens$z))
  # colnames(densdf)[1:2] <- colnames(df)
  
  if(!is.null(fname))
    pdf(fname)
  m <- ggplot(df[idx, ], aes_string(x=cn[1], y=cn[2]))
  m <- m + geom_density2d()
  if (!is.null(yintercept))
    m <- m + geom_hline(yintercept = yintercept) 
  if (!is.null(xintercept))
    m <- m + geom_vline(xintercept = xintercept)
  print(m)
  if(!is.null(fname))
    dev.off()
}
