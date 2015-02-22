#' @name iterativeOcc
#' @title Train a iterative one-class classifier. 
#' @description ...
#' @param P a data frame with the positive training samples.
#' @param U an object of class \code{\link{rasterTiled}} image data to be classified.
#' @param settings the settings list
#' @param PN a data frame used as independent test set. the first column must be the response variable with 1 being the positive and -1 the negative samples.
#' @param overwrite should already computed elements be recalculated
#' @param ... other arguments that can be passed to \code{\link{trainOcc}}
#' @export
iterativeOcc <- function (P, U, settings=NULL, PN=NULL,
                          overwrite=FALSE,
#                           iterMax = 'noChange',
#                           nTrainUn = nrow(P)*2, 
#                           kFolds = 10, 
#                           indepUn = 0.5, 
#                           expand=2, 
#                           baseDir=NULL,
#                           PN=NULL, 
#                           seed=NULL,
#                           scale=TRUE, 
#                           minPppAtLw=FALSE,
                          ...
) {
  

  if (is.null(settings$baseDir)) {
    settings$baseDir <- paste(tempdir(), "\\iterOneClass", sep="")
    cat("\nSave results in ", settings$baseDir, "\n\n")
  }

  attach(settings)

  if (exists_fname("ini", baseDir) & !overwrite)
  
  settings$colors_pu<- list(pos='#2166ac', un='#e0e0e0') 
  settings$colors_pn <- list(pos='#2166ac', neg='#f4a582')
  
  if (!is.null(PN)) {
    #   index_test <- match(as.numeric(rownames(PN)), 
    #                                  U$validCells)
    pred_neg_test <- vector(mode="integer", length=nrow(PN))
  } else {
    pred_neg_test <- NULL
    PN <- NULL
  }
  
  n_un <- length(U$validCells)
  n_un_all_iters <- n_un
  nPixelsPerTile <- U$tiles[2,1]
  
  if (!is.null(seed))
    seed_global <- settings$seed
  
  dir.create(baseDir, showWarnings=FALSE)
  
  pred_neg <- vector(mode="integer", length=n_un)
  
  validCells <- U$validCells
  
  save(n_un, nPixelsPerTile, validCells, seed_global, 
       settings, PN, file=get_filename("ini", baseDir))
  
  attach(settings)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ### iterate 
  iter = 0
  STOP = FALSE
  
  time_stopper <- proc.time()
  time_stopper_model <- time_stopper_predict <- c()
  
  
  while(!STOP) {
    
    iter <- iter + 1
    n_un_iter <- length( U$validCells )
    
    
    if (!is.null(seed)) {
      set.seed(seed_global*iter)
      seed <- round(runif(1, 1, 1000000), 0)
    }
    
    cat(sprintf("Iteration: %d \n\tPercent unlabeled: %2.2f", iter, (n_un_iter/n_un)*100 ), "\n")
    
    ### classification
    cat("\tTraining ... ")
    ans <- proc.time()
    train_un <- sample_rasterTiled(U, size=nTrainUn, seed=seed)
    train_pu_x <- rbind(P, train_un)
    train_pu_y <- puFactor(rep(c(1, 0), 
                               c(nrow(P), nrow(train_un))))
    
    index <- createFoldsPu( train_pu_y, kFolds=kFolds, 
                            indepUn=indepUn, seed=seed )
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
    pred <- predict(U, model, returnRaster = FALSE, 
                    fnameLog = iocc_filename(baseDir, iter, 
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
    
    
    if (!is.null(PN)) {
      cat("\tEvaluation ... ")
      dummy <- .evaluate_iocc(model, PN, pred_neg_test, th, iter)
      pred_test <- dummy$pred_test
      pred_neg_test <- dummy$pred_neg_test
      ev <- dummy$ev
    }
    
    ### plot diagnostics
    cat("\n\tWriting results in folder ", baseDir)
      
    param_as_str <- paste(colnames(signif(model$bestTune)), 
                          model$bestTune, sep=": ", 
                          collapse=" | ")
    
    ### grid
    ans <- plot(model, plotType="level")
    pdf(iocc_filename(baseDir, iter, "_grid.pdf"))
    trellis.par.set(caretTheme())
    print(ans)
    dev.off()
    
    ### histogram
    pdf(iocc_filename(baseDir, iter, "_histogram.pdf"))
    hist(model, pred, main=param_as_str)
    abline(v=c(th, attr(th, "th_non_expanded")))
    if (!is.null(PN)) {
      rug(pred_test[PN$y==1], ticksize = 0.03, col=colors_pn$p)
      rug(pred_test[PN$y==-1], ticksize = -0.03, col=colors_pn$n)
    }
    dev.off()
    ### featurespace
    if (ncol(train_pu_x)==2) {
      pdf(iocc_filename(baseDir, iter, "_featurespace.pdf"))
      featurespace(model, 
                   thresholds=c(th, attr(th, "th_non_expanded")), 
                   main=param_as_str)
      dev.off()
    }
    
    ### test
    if (!is.null(PN)) {
      pdf(iocc_filename(baseDir, iter, "_eval_pn.pdf"))
      plot(ev, main=paste("max. Kappa:", round(max(ev@kappa), 2)))
      dev.off()
    }
    
    ### change plots
    dff <- abs(diff(n_un_all_iters))
    
    pdf(iocc_filename(baseDir, 0, "_n_unlabeled.pdf"))
      plot(1:(iter+1), n_un_all_iters, type="b", 
           xlab="iteration", ylab="# unlabeled", log="y")
      text(2:(iter+1), n_un_all_iters[2:(iter+1)], 
           label = paste("d:", dff), pos=c(3))    
    dev.off()
    pdf(iocc_filename(baseDir, 0, "_time.pdf"))
      plot(1:(iter), diff(time_stopper[, "elapsed"]), 
           type="b", xlab="iteration", 
           ylab="time [s]")
    dev.off()
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # update U
    U <- rasterTiled(U$raster, 
                      mask=U$validCells[pred>=th], 
                      nPixelsPerTile = nPixelsPerTile)
    
    ### save results of this iteration
    save(iter, model, pred, th, pred_neg, pred_neg_test, ev, 
         n_un_all_iters, seed_global, seed, 
         time_stopper, time_stopper_model, time_stopper_predict,
         file=iocc_filename(baseDir, iter, "_results.RData") )
    
    if (is.character(iterMax)) {
      # iter of no change
      if (grep("+", iterMax)==1) {
        nSeqNoChangeCrit <- as.numeric(strsplit(iterMax, "[+]")[[1]][2])
        nSeqNoChange <- sum(dff[ max(1, iter-nSeqNoChangeCrit+1) : iter ] == 0)
        if (nSeqNoChange >= nSeqNoChangeCrit)
          STOP=TRUE
      }
      if ( iterMax=="noChange" & 
             dff[iter]==0 )
        STOP=TRUE
    } else if (is.numeric(iter)) {
      if (iter==iterMax)
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
    list(U = U,
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
         file=iocc_filename(baseDir, iter, "_results.RData")
    )
  )
}