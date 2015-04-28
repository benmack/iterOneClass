#' @importFrom dismo evaluate
#' @export
.evaluate_iocc <- function (model, test_set, pred_neg_test, th, iter) {
  n_un_test_iter <- sum(pred_neg_test==0)
  cat(sprintf("\tPercent unlabeled (test): %2.2f", 
              (n_un_test_iter/nrow(test_set))*100 ), "\n")
  
  pred_in_pred_neg_test <- which(pred_neg_test==0)
  pred_test <- predict(model,
                       test_set[pred_in_pred_neg_test, -1])
  new_neg_in_pred_neg_test <- pred_in_pred_neg_test[pred_test<th]
  pred_neg_test[ new_neg_in_pred_neg_test ] <- iter
  
  ans <- pred_test
  pred_test <- rep(0+min(ans), nrow(test_set))
  pred_test[pred_in_pred_neg_test] <- ans
  
  ev <- evaluate(p = pred_test[test_set$y==1], 
                 a = pred_test[test_set$y==-1])
  
  return(list(pred_test=pred_test, 
              pred_neg_test=pred_neg_test, 
              ev=ev))
}