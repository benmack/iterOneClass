#' @name thresholdLikN
#' @title A conservative threshold for classifying highly likely negatives.
#' @description ... 
#' @param model an object of class \code{\link{trainOcc}}.
#' @param pred the predictions of the unlabeled samples. 
#' @param method a thresholding method. So far only \code{lw} (lower whisker) and \code{min} are implemented.
#' @param expand change the threshold such that \code{n_un_th*expand} pixels remain unlabeled, where \code{n_un_th} is the number of samples (of \code{pred}) remaining unlabeled when applying the threshold determined by \code{method}.
#' @export
thresholdLikN <- 
  function(model, pred, thStart="lw", expand=2) {
    hops <- holdOutPredictions(model, aggregate=TRUE)$pos
    if (thStart=="lw") {
      bp <- boxplot( hops, plot=FALSE)
      threshold <- bp$stats[[1]]
    } else if (thStart=="min") {
      threshold <- min(hops)
    } else {
      stop("\'thstart\' must be \'lw\' or \'min\'.")
    }
    n.pos <- sum(pred >= threshold)
    n.remaining.un <- n.pos * expand
    n.remaining.un <- min(n.remaining.un, length(pred))
    frac.remaining.un <- n.remaining.un/length(pred)
    th.classify.neg <- quantile( pred, probs =  1-frac.remaining.un )
    attr(th.classify.neg, "th_non_expanded") <- threshold
    return(th.classify.neg)
  }
