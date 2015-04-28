#' @export
get_puSet <- function(base_dir, iter, modRow=NULL, unSub=NULL, seed=NULL,
                      folder_U=NULL, verbose=FALSE, model=NULL) {
  
  .get_puSet <- function(x, base_dir, iter, mdRw, unSub=NULL, seed=NULL,
                         folder_U=NULL, verbose=FALSE) {
    
    x <- update(x, modRow=mdRw)
    hop <- holdOutPredictions(x, aggregate=TRUE)
    
    trSet_pos <- x$trainingData[x$trainingData[,ncol(x$trainingData)]=="pos", 
                                -ncol(x$trainingData)]
    pos_cal <- predict(x, trSet_pos)
    hop$pos_cal <- pos_cal
    
    if (!is.null(folder_U)) {
      fname_pred <- get_fname("predU_data", base_dir, iter=iter, 
                              modRow=mdRw, subfolder=folder_U)
      if (file.exists(fname_pred)) {
        load(fname_pred)
      } else {
        pred <- predict_Usub(base_dir, iter, unSub, modRows=mdRw, 
                             seed=seed, folder_suffix=folder_U)
      }
      hop$un <- pred
    }
    return(hop)
  }
  
  if (is.null(model))
      model <- get_model(base_dir=base_dir, iter=iter)
  if (is.null(modRow))
    modRow <- 1:nrow(model$results)
  
  if (length(modRow)==1) {
    hop <- .get_puSet(x=model, base_dir=base_dir, iter=iter, mdRw=modRow, 
                      unSub=unSub, seed=seed, folder_U=folder_U, 
                      verbose=verbose)
  } else {
    hop <- foreach(mr = modRow, 
                   .packages=c("oneClass", "kernlab"), .verbose=FALSE) %dopar%
      .get_puSet(x=model, base_dir=base_dir, iter=iter, mdRw=mr, 
                 unSub=unSub, seed=seed, folder_U=folder_U, 
                 verbose=verbose)
  }
  return(hop)
}
