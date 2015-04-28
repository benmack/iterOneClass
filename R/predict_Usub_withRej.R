#' @export
predict_Usub_withRej <- function(iocc, iter, unSub=5000, 
                         modRows=NULL, seed=NULL, 
                         fname_image=NULL,
                         folder_suffix=NULL, 
                         model=NULL) {
  
  model_new <- model
  rm(model)
  # REFACTOR: ALSO IN plot_iocc
  # - ADDED:
  modRow = NULL
  # - |
  if (is.character(iocc)) {
    iocc <- get_ioccObj(iocc, iter, modRow=modRow)
    baseDir <- iocc$baseDir
    if (is.null(iter))
      iter <- .nIter(baseDir)
  } else {
    baseDir = iocc$baseDir
    if (is.null(iter)) {
      iter <- iocc$iter
    } else {
      if (iter != iocc$iter)
        iocc <- get_ioccObj(iocc$baseDir, iter, modRow=modRow)
    }
  }
  iocc.i <- iocc
  rm(iocc)
  
  if (!is.null(model_new)) {
    iocc.i$model <- model_new
  }
    
  if (length(unSub) == 1) {
    nUnSub <- unSub
  } else {
    nUnSub <- nrow(unSub)
  }
  
  #   if (is.null(fname_image))
  #     fname_image <- iocc$U$raster@file@name
  if (is.null(folder_suffix))
    folder_suffix <- paste0("predU-", nUnSub, "_s", seed)
  
  outdir <- get_fname("predU_U", baseDir, iter, subfolder=folder_suffix,
                      dirOnly=TRUE)
  outdir_fig <- get_fname("perf_hist", baseDir, iter, modRow=1,
                          folder_suffix, dirOnly=TRUE)
  
  
  # dir.create(outdir, recursive = TRUE, showWarnings=FALSE)
  
  iocc.i <- get_ioccObj(baseDir, iter)
  if (!is.null(model_new))
    iocc.i$model <- model_new

  # , filename_U=fname_image, U_as_df=FALSE  # U_as_df=TRUE for building hist with all unlabeled samples
  if (is.null(modRows)) {
    modRows <- 1:nrow(iocc.i$model$results)
  }
  # check which ones are already available
  x_pred_list <- dir(outdir)[grep("_pred.", dir(outdir))]
  if (length(x_pred_list)>0) {
    available <- sort(sapply(x_pred_list, 
                             function(x) as.numeric(substr(x, 1, 3))))
    modRows <- modRows[!(modRows %in% available)]
  }
  
  if (length(modRows) == 0) {
    cat("All predictions already computed.\n")
    return(NULL)
  }
  
  ### --------------------------------------------------------
  ### get histogram plots of all models 
  # outdir <- paste0(iocc_filename(base_dir=baseDir, 
  #                                iter=iter), "/predU-", unSub, "/")
  # dir.create(outdir, recursive = TRUE, showWarnings=FALSE)
  
  fname <- get_fname("predU_U", baseDir, iter, folder_suffix)
  
  U_df <- get_Usub(baseDir, iter, unSub, seed, folder_suffix)
  
  
  .predUreject <- function(U_df, mod, th, likN, baseDir, ii, folder_suffix) {
    fname <- get_fname("predU_data", baseDir, ii, modRow=0,
                 subfolder=folder_suffix)
    if (!file.exists(fname)) {
      pred.ii <- predict(mod, U_df)
      likN.ii <- pred.ii<th
      likN <- likN | likN.ii
      pred.ii[likN] <- min(pred.ii[!likN])
      save(likN, likN.ii, pred.ii, file=fname)
    } else {
      load(fname)
    }
    return(list(pred.ii=pred.ii, likN.ii=likN.ii, likN=likN))
  }

  if (is.null(iocc.i$th_all))
    stop("iocc.i$th_all is missing.")
  
  all_ths <- iocc.i$th_all
  all_mods <- list()
  for (iii in 1:(iter)) {
    all_mods[[iii]] <- get_model(baseDir, iii)
  }
  
  likN_iter <- logical(nrow(U_df))
  
  if (iter > 1) {
    for (ii in 1:(iter-1)) {
      likN_iter <- .predUreject(U_df, 
                                all_mods[[ii]], all_ths[ii], 
                                likN_iter, baseDir, ii, folder_suffix)$likN
      cat(sum(likN_iter),".")
    }
    cat("\n")
    U_df <- U_df[!likN_iter, ]
  }
  

  ### --------------------------------------------------------
  ### get histogram plots of all models 
  predict_U <- function(modRow, iter, model, x_un, likN_iter, baseDir, subfolder) {
    cat(modRow, ".")
    fname <- get_fname("predU_data", baseDir, iter, modRow,
                       subfolder=subfolder)
    if (!file.exists(fname)) {
      model <- update(model, modRow=modRow)
      pred <- predict(model, x_un)
      if (any(likN_iter)) {
        pred_sub <- pred
        pred <- numeric(length(likN_iter))+min(pred_sub)
        pred[!likN_iter] <- pred_sub
      }
      save(pred, file=fname)
    } else {
      load(fname)
    }
  }
  
  if (iter == 2)
    a=3
  
  foreach(modRow = modRows, 
          .packages=c("oneClass", "kernlab"), 
          .verbose=FALSE) %do% predict_U(modRow=modRow, iter=iter, 
                                         model=iocc.i$model, 
                                         x_un=U_df, likN_iter=likN_iter, 
                                         baseDir=baseDir, 
                                         subfolder=folder_suffix)
  return(NULL)
}
#   foreach(mm = modRows, 
#           .packages=c("oneClass", "kernlab"), 
#           .verbose=TRUE) %dopar% write_hist(iocc.i$model, 
#                                             x_un=U_df,  # iocc.i$U_df
#                                             modRows=mm, 
#                                             thDepPerf=TRUE, 
#                                             folder_out=outdir_fig,
#                                             format='pdf')
