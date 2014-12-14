one_step_occ <- function(sttngs, iter, filename_U, test_set,
                         folder_out, allowParallel=TRUE) {
  
  time_stopper_model <- time_stopper_predict <- c()
  
  fname_model <- paste0(folder_out, "iter-", iter, "_model.RData")
  dir.create(folder_out)
  
  
  iocc.i <- iocc_load(sttngs$baseDir, iter,
                      filename_U=filename_U)
  un <- rasterTiled(brick(iocc.i$filename_U), mask=iocc.i$validCells)
  idx.feats <- colnames(iocc.i$model$trainingData)!=".outcome"
  names(un$raster) <- colnames(iocc.i$model$trainingData)[idx.feats]
  
  if (!file.exists(fname_model)) {
    
    idx.obs <- iocc.i$model$trainingData$.outcome=="pos"
    train_pos <- iocc.i$model$trainingData[idx.obs, idx.feats]
    
    n_un_bsvm <- as.integer(corresponding_samplesize_in_U(iocc.i$n_un,
                                                          iocc.i$n_un_iter,
                                                          sttngs$nTrainPos))
    train_un <- sample_rasterTiled(un, n_un_bsvm, seed=sttngs$seed)
    train_pu_x <- rbind(train_pos, train_un)
    train_pu_y <- puFactor(rep(c(1, 0), c(nrow(train_pos),
                                          nrow(train_un))))
    index <- createFoldsPu(train_pu_y, k=sttngs$kFolds,
                           indepUn=sttngs$indepUn, seed=sttngs$seed)
    
    cat("\tTraining ... ")
    ans <- proc.time()
    ### due to memory issues:
    model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index,
                      tuneGrid=sttngs$tuneGrid, allowParallel = allowParallel)
    time_stopper_model <- rbind(time_stopper_model, (proc.time()-ans)[1:3])
    save(train_pu_x, train_pu_y, n_un_bsvm, index, model, 
         time_stopper_model, file=fname_model)
  } else {
    load(fname_model)
  }
  
  modRow <- modelPosition(model)$row
  fname_pred <- gsub("model", paste0("model-", modRow, "_pred"),
                     fname_model)
  
  if (!file.exists(fname_pred)) {
    
    cat("\tPrediction ... ")
    ans <- proc.time()
    pred <- predict(un, model, returnRaster = FALSE, 
                    fnameLog = gsub(".RData", "_LOG.txt", 
                                    fname_pred) )
    time_stopper_predict <- rbind(time_stopper_predict, (proc.time()-ans)[1:3])
    save(pred, time_stopper_predict, file=fname_pred)
  } else {
    load(fname_pred)
  }
  
  if (!is.null(test_set)) {
    fname_eval <- gsub("model", paste0("model-", modRow, "_eval"),
                       fname_model)
    if (!file.exists(fname_eval)) {
      cat("\tEvaluation ... ")
      pred_test <- predict(model, test_set[, -1])
      ev <- evaluate(p = pred_test[test_set$y==1], 
                     a = pred_test[test_set$y==-1])
      save(pred_test, ev, file=fname_eval)
    } else {
      load(fname_eval)
    }
  }
  
  ### Some comparison plots
  fname_plot = gsub("model", paste0("model-", modRow, "_WHAT"),
                    fname_model)
  fname_plot = gsub(".RData", ".pdf", fname_plot)
  
  ###
  if (ncol(train_pu_x)==2){
    pdf(file=gsub("WHAT", "featurespace_iocc", fname_plot))
    featurespace(iocc.i$model)
    dev.off()
    pdf(file=gsub("WHAT", "featurespace_bench", fname_plot))
    featurespace(model)
    dev.off()
  }
  
  ### Hist plot and ev of benchmark
  pdf(file=gsub("WHAT", "histUev_bench", fname_plot))
  yLimits = histUev(model, ev, pred=pred)
  abline(h=max(yLimits)*max(iocc.i$ev@kappa), lwd=2)
  dev.off()
  ### Hist plot and ev of iocc
  pdf(file=gsub("WHAT", "histUev_iocc", fname_plot))
  yLimits = histUev(iocc.i$model, iocc.i$ev, pred=iocc.i$pred)
  abline(h=max(yLimits)*max(ev@kappa), lwd=2)
  dev.off()
  
  ### Comparing predictions
  pred_iocc_all <- get_full_pred_vector(iocc.i)
  hist2d(pred, pred_iocc_all, fname=gsub("WHAT", "hist2d", 
                                         fname_plot))
  
  
  
  #   ### AUCs - TEST PU
  #   ### This does not make a lot of sense, the curves are too similar!
  #   library(pROC)
  #   roc_occ <- plot.roc(test_set$y==1, pred_test,
  #                       main="Statistical comparison", percent=TRUE, col="#1c61b6")
  #   roc_iocc <- lines.roc(test_set$y==1, iocc.i$pred_neg_test, 
  #                        percent=TRUE, col="#008600")
  #   testobj <- roc.test(roc_occ, roc_iocc)
  #   text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
  #   legend("bottomright", legend=c("OCC", "iterOCC"), 
  #          col=c("#1c61b6", "#008600"), lwd=2)
}