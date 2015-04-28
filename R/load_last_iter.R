#'@export
load_last_iter <- function(sttngs, last="first_noChange", 
                           filename_U=NULL) {
  if (is.numeric(last)) {
    iter_last = last
    n_un_all_iters = NULL
  } else if (last=="first_noChange") {
    iocc.nIter <- length(grep("results.RData", 
                              dir(sttngs$baseDir)))
    iocc.i <- get_ioccObj(sttngs$baseDir, iocc.nIter, 
                        filename_U=filename_U, 
                        U_as_df=FALSE)
    nU <- length(iocc.i$pred_neg)
    dff <- sapply(1:iocc.nIter, function(i) sum(iocc.i$pred_neg==i, na.rm=TRUE))
    n_un_all_iters <- c(nU, nU-cumsum(dff))
    dff <- abs(diff(n_un_all_iters))
    iter_last <- which(dff==0)[1]
  }
  iocc.i <- get_ioccObj(sttngs$baseDir, iter_last, 
                      filename_U=filename_U, U_as_df=FALSE)
  return(list(iocc=iocc.i, n_un_all_iters=n_un_all_iters, iter_last=iter_last))
}
