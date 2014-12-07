#' @name iterativeOcc
#' @title Train a iterative one-class classifier. 
#' @description ...
#' @param train_pos a data frame with the positive training samples.
#' @param un an object of class \code{\link{rasterTiled}} image data to be classified.
#' @param iter_max maximum number of iterations.
#' @param n_train_un the number of unlabeled samples to be used for training and validation.
#' @param k the number of folds used for resampling.
#' @param indep_un the fraction of unlabeled samples used for validation.
#' @param expand ...
#' @param base_dir a folder where the results are written to.
#' @param test_set a data frame used as independent test set. the first column must be the response variable with 1 being the positive and -1 the negative samples.
#' @param seed a seed point to be set for sampling the unlabeled data and the
#' @param ... other arguments that can be passed to \code{\link{trainOcc}}
#' @export
iterativeOcc <- function (train_pos, un, 
                          iter_max = 'noChange',
                          n_train_un = nrow(train_pos)*2, 
                          k = 10, indep_un = 0.5, 
                          expand=2, 
                          base_dir=NULL,
                          test_set=NULL, 
                          seed=NULL,
                          scale=TRUE, 
                          minPppAtLw=FALSE,
                          ...
) {
  
  if (is.null(base_dir)) {
    base_dir <- paste(tempdir(), "\\iterOneClass", sep="")
    cat("\nSave results in ", base_dir, "\n\n")
  }
  
  colors_pu<- list(pos='#2166ac', un='#e0e0e0') 
  colors_pn <- list(pos='#2166ac', neg='#f4a582')
  
  if (!is.null(test_set)) {
    #   index_test <- match(as.numeric(rownames(test_set)), 
    #                                  un$validCells)
    pred_neg_test <- vector(mode="integer", length=nrow(test_set))
  }
  
  n_un <- length(un$validCells)
  n_un_all_iters <- n_un
  nPixelsPerTile <- un$tiles[2,1]
  
  if (!is.null(seed))
    seed_global <- seed
  
  dir.create(base_dir, showWarnings=FALSE)
  
  pred_neg <- vector(mode="integer", length=n_un)
  
  validCells <- un$validCells
  
  save(n_un, nPixelsPerTile, validCells, seed_global, 
       file=iocc_filename(base_dir, 0, "_ini.RData") )
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ### iterate 
  iter = 0
  STOP = FALSE
  
  time_stopper <- proc.time()
  time_stopper_model <- time_stopper_predict <- c()
  
  
  while(!STOP) {
    
    iter <- iter + 1
    n_un_iter <- length( un$validCells )
    
    
    if (!is.null(seed)) {
      set.seed(seed_global*iter)
      seed <- round(runif(1, 1, 1000000), 0)
    }
    
    cat(sprintf("Iteration: %d \n\tPercent unlabeled: %2.2f", iter, (n_un_iter/n_un)*100 ), "\n")
    
    ### classification
    cat("\tTraining ... ")
    ans <- proc.time()
    train_un <- sample_rasterTiled(un, size=n_train_un, seed=seed)
    train_pu_x <- rbind(train_pos, train_un)
    train_pu_y <- puFactor(rep(c(1, 0), 
                               c(nrow(train_pos), nrow(train_un))))
    
    index <- createFoldsPu( train_pu_y, k=k, 
                            indepUn=indep_un, seed=seed )
    model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index, ...)

    if (minPppAtLw) {
      cat("threshold at lower-whisker ... ")
      # if (oneClass:::.foreach.exists()) { # ASSUMED
        newMetrics <- foreach(mm=1:nrow(model$results),
                              .combine=rbind,
                              .packages="oneClass") %dopar%
          pppAtLowerWhisker(model, modRow=mm)
      
      
      newMetrics <- data.frame(newMetrics, 1-newMetrics[, 2])
      colnames(newMetrics) <- c('thAtLw', 'pppAtLw', 'pnpAtLw')
      
      model$results <- cbind(model$results, newMetrics)
      
      model <- update(model, modRank=1, metric="pnpAtLw")
      
      #hist(model)
      #featurespace(model, thresholds=newMetrics[idx, 1])
   }
    
    time_stopper_model <- rbind(time_stopper_model, (proc.time()-ans)[1:3])
    cat("Completed in", time_stopper_model[iter, 3], "sec.\n")
    
    
    cat("\tPrediction ... ")
    ans <- proc.time()
    pred <- predict(un, model, returnRaster = FALSE, 
                    fnameLog = iocc_filename(base_dir, iter, 
                                             "_log_prediction.txt"))
    
    time_stopper_predict <- rbind(time_stopper_predict, (proc.time()-ans)[1:3])
    cat("Completed in", time_stopper_predict[iter, 3], "sec.\n")
    
    th <- thresholdNu(model, pred, expand=expand)
  
    pred_in_pred_neg <- which(pred_neg==0)
    new_neg_in_pred_neg <- pred_in_pred_neg[pred<th]
    pred_neg[ new_neg_in_pred_neg ] <- iter
    
    n_un_all_iters[iter+1] <- n_un_all_iters[iter]-length(new_neg_in_pred_neg)
    
    ### time
    time_stopper <- rbind(time_stopper, proc.time())
    
    
    if (!is.null(test_set)) {
      cat("\tEvaluation ... ")
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
    }
    
    ### plot diagnostics
    cat("\n\tWriting results in folder ", base_dir)
      
    param_as_str <- paste(colnames(signif(model$bestTune)), 
                          model$bestTune, sep=": ", 
                          collapse=" | ")
    
    ### grid
    ans <- plot(model, plotType="level")
    pdf(iocc_filename(base_dir, iter, "_grid.pdf"))
    trellis.par.set(caretTheme())
    print(ans)
    dev.off()
    
    ### histogram
    pdf(iocc_filename(base_dir, iter, "_histogram.pdf"))
    hist(model, pred, main=param_as_str)
    abline(v=c(th, attr(th, "th_non_expanded")))
    if (!is.null(test_set)) {
      rug(pred_test[test_set$y==1], ticksize = 0.03, col=colors_pn$p)
      rug(pred_test[test_set$y==-1], ticksize = -0.03, col=colors_pn$n)
    }
    dev.off()
    ### featurespace
    if (ncol(train_pu_x)==2) {
      pdf(iocc_filename(base_dir, iter, "_featurespace.pdf"))
      featurespace(model, 
                   thresholds=c(th, attr(th, "th_non_expanded")), 
                   main=param_as_str)
      dev.off()
    }
    
    ### test
    if (!is.null(test_set)) {
      pdf(iocc_filename(base_dir, iter, "_eval_pn.pdf"))
      plot(ev, main=paste("max. Kappa:", round(max(ev@kappa), 2)))
      dev.off()
    }
    
    ### change plots
    dff <- abs(diff(n_un_all_iters))
    
    pdf(iocc_filename(base_dir, 0, "_n_unlabeled.pdf"))
      plot(1:(iter+1), n_un_all_iters, type="b", 
           xlab="iteration", ylab="# unlabeled", log="y")
      text(2:(iter+1), n_un_all_iters[2:(iter+1)], 
           label = paste("d:", dff), pos=c(3))    
    dev.off()
    pdf(iocc_filename(base_dir, 0, "_time.pdf"))
      plot(1:(iter), diff(time_stopper[, "elapsed"]), 
           type="b", xlab="iteration", 
           ylab="time [s]")
    dev.off()
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # update un
    un <- rasterTiled(un$raster, 
                      mask=un$validCells[pred>=th], 
                      nPixelsPerTile = nPixelsPerTile)
    
    ### save results of this iteration
    save(iter, model, pred, th, pred_neg, pred_neg_test, ev, 
         n_un_all_iters, seed_global, seed, 
         time_stopper, time_stopper_model, time_stopper_predict,
         file=iocc_filename(base_dir, iter, "_results.RData") )
    
    if (is.character(iter_max)) {
      # iter of no change
      if (grep("+", iter_max)==1) {
        nSeqNoChangeCrit <- as.numeric(strsplit(iter_max, "[+]")[[1]][2])
        nSeqNoChange <- sum(dff[ max(1, iter-nSeqNoChangeCrit+1) : iter ] == 0)
        if (nSeqNoChange >= nSeqNoChangeCrit)
          STOP=TRUE
      }
      if ( iter_max=="noChange" & 
             dff[iter]==0 )
        STOP=TRUE
    } else if (is.numeric(iter)) {
      if (iter==iter_max)
        STOP = TRUE  
    } else {
      rm(model, pred, th, ev, seed, n_un_iter, train_un,
       train_pu_x, train_pu_y)
    }
    
    
    cat(sprintf("\t%2.2f real and %2.2f CPU time required.", 
                (time_stopper[iter+1, , drop=FALSE]-
                   time_stopper[iter, , drop=FALSE])[,"elapsed"], 
                (time_stopper[iter+1, , drop=FALSE]-
                   time_stopper[iter, , drop=FALSE])[,"sys.self"]
    ), "\n")
  }
  
  return( 
    list(un = un,
         iter=iter, 
         model=model, 
         pred=pred, 
         th=th, 
         pred_neg=pred_neg, 
         pred_neg_test=pred_neg_test, 
         ev=ev, 
         seed_global=seed_global, 
         seed=seed, 
         time_stopper=time_stopper, 
         time_stopper_model=time_stopper_model, 
         time_stopper_predict=time_stopper_predict,
         file=iocc_filename(base_dir, iter, "_results.RData")
    )
  )
}