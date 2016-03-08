#' @export
evaluate_iocc <- function(baseDir, iters, refset, 
                          modRows=NULL, subfolder=NULL, 
                          saveplot=TRUE, fname_U=NULL) {
  
  if (!is.null(modRows) & !is.list(modRows))
    stop("\'modRows\' must be a named list.")
  
  if (length(iters)>1 & 
        !(length(iters) == length(modRows) | is.null(modRows)))
    stop("If length of iters is > 1, modrows must be NULL or a list of length length(iters).")
  
  iname <- function(itr)
    return(paste0("iter", itr))
  
  # Make sure all predictions are availabel
  for (i in iters) {
    cat("Predicting data for iteration", i, "\n")
    if (length(iters)==1 & !is.list(modRows)) {
      predict_Usub_withRej(baseDir, iter=i, unSub=refset[, -1],
                         modRows=modRows[[iname(i)]], 
                         folder_suffix=subfolder,
                         fname_U=fname_U)
    } else {
      predict_Usub_withRej(baseDir, iter=i, unSub=refset[, -1],
                         modRows=modRows[[iname(i)]], 
                         folder_suffix=subfolder,
                         fname_U=fname_U)
    }
  }
  
  .evaluate <- function(baseDir, iter, modRow, subfolder, y, saveplot) {
    fname_ev <- get_fname("eval", baseDir, iter, modRow,
                          subfolder=subfolder)
    if (!file.exists(fname_ev)) {
      # predictions
      fname <- get_fname("predU_data", baseDir, iter, modRow,
                         subfolder=subfolder)
      load(fname)
      ev <- evaluate(p = pred[y==1],
                     a = pred[y==-1])
      save(ev, file=fname_ev)
    } else {
      load(fname_ev)
    }
    if (saveplot) {
      fname_ev <- get_fname("eval_plot", baseDir, iter, modRow,
                      subfolder=subfolder)
      if (!file.exists(fname_ev)) {
        pdf(fname_ev)
          plot(ev)
        dev.off()
      }
    }
    return(ev)
  }
  
  ev_all <- list()
  counter <- 0
  
  for (i in iters) {
    counter <- counter + 1
    cat("Evaluation: iteration", i, "\n")
    ev_all[[counter]] <- foreach(modRow = modRows[[iname(i)]],
              .packages=c("dismo"), .verbose=FALSE) %do% 
      .evaluate(baseDir, i, modRow, subfolder, refset$y, saveplot)
    names(ev_all[[counter]]) <- paste0("modRow", modRows[[iname(i)]])
  }
  names(ev_all) <- paste0('iter', iters)
  #if (length(iters)==1)
  #  ev_all <- ev_all[[1]]
  
  return (ev_all)
}