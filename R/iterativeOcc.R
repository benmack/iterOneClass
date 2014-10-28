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
#' @param folder_out a folder where the results are written to.
#' @param test_set a data frame used as independent test set. the first column must be the response variable with 1 being the positive and -1 the negative samples.
#' @param seed a seed point to be set for sampling the unlabeled data and the
#' @param ... other arguments that can be passed to \code{\link{trainOcc}}
#' @export
iterativeOcc <- function (train_pos, un, 
                          iter_max = 'noChange',
                          n_train_un = nrow(train_pos)*2, 
                          k = 10, indep_un = 0.5, 
                          expand=2, 
                          folder_out=NULL,
                          test_set=NULL, 
                          seed=NULL,
                          scale=TRUE, 
                          ...
) {
  
  if (is.null(folder_out)) {
    folder_out <- paste(tempdir(), "\\iterOneClass", sep="")
    cat("\nSave results in ", folder_out, "\n\n")
    dir.create(folder_out, showWarnings=FALSE)
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
  
  dir.create(folder_out)
  
  pred_neg <- vector(mode="integer", length=n_un)
  
  validCells <- un$validCells
  
  save(n_un, nPixelsPerTile, validCells, seed_global, .fname, 
       file=.fname(folder_out, 0, ".RData", "initialized") )
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ### iterate 
  iter = 0
  STOP = FALSE
  
  time_stopper <- proc.time()
  while(!STOP) {
    
    iter <- iter + 1
    n_un_iter <- length( un$validCells )
    
    
    if (!is.null(seed)) {
      set.seed(seed_global*iter)
      seed <- round(runif(1, 1, 1000000), 0)
    }
    
    cat(sprintf("Iteration: %d \n\tPercent unlabeled: %2.2f", iter, (n_un_iter/n_un)*100 ), "\n")
    
    ### classification
    cat("\tTraining ...\n")
    train_un <- sample_rasterTiled(un, size=n_train_un, seed=seed)
    train_pu_x <- rbind(train_pos, train_un)
    train_pu_y <- puFactor(rep(c(1, 0), 
                               c(nrow(train_pos), nrow(train_un))))
    
    index <- createFoldsPu( train_pu_y, k=k, 
                            indepUn=indep_un, seed=seed )
    
    model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index, ...)
    
    cat("\tPrediction ...\n")
    pred <- predict(un, model, returnRaster = FALSE, 
                    fnameLog = paste(paste(folder_out, 
                                           "/log_prediction_iter-", 
                                           iter, ".txt", sep="") ) )
    
    th <- thresholdNu(model, pred, expand=expand)
    
    pred_in_pred_neg <- which(pred_neg==0)
    new_neg_in_pred_neg <- pred_in_pred_neg[pred<th]
    pred_neg[ new_neg_in_pred_neg ] <- iter
    
    n_un_all_iters[iter+1] <- n_un_all_iters[iter]-length(new_neg_in_pred_neg)
    
    ### time
    time_stopper <- rbind(time_stopper, proc.time())
    
    
    if (!is.null(test_set)) {
      cat("\tEvaluation ...\n")
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
    param_as_str <- paste(colnames(signif(model$bestTune)), 
                          model$bestTune, sep=": ", 
                          collapse=" | ")
    
    ### grid
    ans <- plot(model, plotType="level")
    pdf(.fname(folder_out, iter, ".pdf", "grid"))
    trellis.par.set(caretTheme())
    print(ans)
    dev.off()
    
    ### histogram
    pdf(.fname(folder_out, iter, ".pdf", "histogram"))
    hist(model, pred, main=param_as_str)
    abline(v=c(th, attr(th, "th_non_expanded")))
    if (!is.null(test_set)) {
      rug(pred_test[test_set$y==1], ticksize = 0.03, col=colors_pn$p)
      rug(pred_test[test_set$y==-1], ticksize = -0.03, col=colors_pn$n)
    }
    dev.off()
    ### featurespace
    if (ncol(train_pu_x)==2) {
      pdf(.fname(folder_out, iter, ".pdf", "featurespace"))
      featurespace(model, 
                   thresholds=c(th, attr(th, "th_non_expanded")), 
                   main=param_as_str)
      dev.off()
    }
    
    ### test
    if (!is.null(test_set)) {
      pdf(.fname(folder_out, iter, ".pdf", "eval_pn"))
      plot(ev, main=paste("max. Kappa:", round(max(ev@kappa), 2)))
      dev.off()
    }
    
    ### change plots
    pdf(.fname(folder_out, 0, ".pdf", "_n_unlabeled"))
      plot(1:(iter+1), n_un_all_iters, type="b", 
           xlab="iteration", ylab="# unlabeled", log="y")
    dev.off()
    pdf(.fname(folder_out, 0, ".pdf", "_time"))
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
         seed_global, seed, time_stopper, 
         file=.fname(folder_out, iter, ".RData", "results") )
    
    if (is.character(iter_max)) {
      if ( iter_max=="noChange" & 
             length(new_neg_in_pred_neg)==0 )
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
         file=.fname(folder_out, iter, ".RData", "results")
    )
  )
}