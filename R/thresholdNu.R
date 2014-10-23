#' @name thresholdNu
#' @title A conservative threshold for classifying highly likely negatives.
#' @description ... 
#' @param model an object of class \code{\link{trainOcc}}.
#' @param pred the predictions of the unlabeled samples. 
#' @param method a thresholding method. So far only \code{lowerWhisker} is implemented.
#' @param expand change the threshold such that \code{n_un_th*expand} pixels remain unlabeled, where \code{n_un_th} is the number of samples (of \code{pred}) remaining unlabeled when applying the threshold determined by \code{method}.
#' @export
thresholdNu <- 
  function(model, pred, method="lowerWhisker", expand=2) {
  if (method=="lowerWhisker") {
    bp <- boxplot( holdOutPredictions(model, aggregate=TRUE)$pos, 
                   plot=FALSE)
    threshold <- bp$stats[[1]]
  }
  n.pos <- sum(pred >= threshold)
  n.remaining.un <- n.pos * expand
  n.remaining.un <- min(n.remaining.un, length(pred))
  frac.remaining.un <- n.remaining.un/length(pred)
  th.classify.neg <- quantile( pred, probs =  1-frac.remaining.un )
  attr(th.classify.neg, "th_non_expanded") <- threshold
  return(th.classify.neg)
}
