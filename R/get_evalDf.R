#' @name get_evalDf
#' @title get_evalDf
#' @export
get_evalDf <- function(evalList, iters=NULL) {
  if (is.null(iters)) {
    iters <- .idFromList(evalList, 'iter')
  }
  df_ev <- data.frame()
  for (i in iters) {
    lname.i <- paste0("iter", i)
    mrs = .idFromList(evalList[[lname.i]], 'modRow')
    mxK <- sapply(evalList[[lname.i]], function(x) max(x@kappa))
    thATmxK <- sapply(evalList[[lname.i]], function(x) x@t[which.max(x@kappa)[1]])
    mxOA <- sapply(evalList[[lname.i]], function(x) max(x@CCR))
    df_ev <- rbind(df_ev,
                   data.frame(iter=rep(i, length(mrs)),
                              modRow=mrs,
                              mxK=mxK,
                              thATmxK=thATmxK,
                              mxOA=mxOA))
  }
  return(df_ev)
}
