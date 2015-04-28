#' @export
.c_ioccObj <- function(env) {
  
  namesList <- c('U', 'iter', 'model', 'pred', 'th', 'th_all',
                 'pred_neg', 'n_un_all_iters',
                 'pred_neg_test', 'ev', 
                 'seed', 
                 'time_stopper', 'time_stopper_model', 
                 'time_stopper_predict', 'baseDir', 'file')
  
  objList <- list()
  for (i in 1:length(namesList)) {
    objList[[i]] <- get(namesList[i], env)
  }
  names(objList) <- namesList
  objList <- structure(objList, class="iocc")
  return(objList)
}