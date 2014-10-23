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
#' @export
iterativeOcc <- function (train_pos, un, 
                          iter_max = 10,
                          n_train_un = nrow(train_pos)*2, 
                          k = 10, indep_un = 0.5, 
                          expand=2, 
                          folder_out="resultsOfIterativeOcc",
                          test_set=NULL, 
                          seed=NULL,
                          scale=TRUE
                          ) {
  
  if (!is.null(test_set)) {
    #   index_test <- match(as.numeric(rownames(test_set)), 
    #                                  un$validCells)
    pred_neg_test <- vector(mode="integer", length=nrow(test_set))
  }
  
  n_un <- length(un$validCells)
  npixelsPerTile <- un$tiles[2,1]
  
  if (!is.null(seed))
    seed_global <- seed

  fname <- 
    function (folder_out, iter, extension=".pdf", ...) {
      paste(folder_out, "/", ..., "_iter",
            formatC(iter, width=2, flag="0"), extension, sep="")
    }
  dir.create(folder_out)
  
  pred_neg <- vector(mode="integer", length=n_un)
  
  validCells <- un$validCells
  
  save(n_un, npixelsPerTile, validCells, seed_global, fname, 
       file=fname(folder_out, 0, ".RData", "initialized") )
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ### iterate 
  iter = 0
  STOP = FALSE
  
  while(!STOP) {
    
    iter <- iter + 1
    n_un_iter <- length( un$validCells )
    
    
    if (!is.null(seed)) {
      set.seed(seed_global*iter)
      seed <- round(runif(1, 1, 1000000), 0)
    }
    
    sprintf("Iteration: %d / Percent unlabeled: %2.2f", iter, (n_un_iter/n_un)*100 )
    
    ### classification
    train_un <- sample_rasterTiled(un, size=n_train_un, seed=seed)
    train_pu_x <- rbind(train_pos, train_un)
    train_pu_y <- puFactor(rep(c(1, 0), 
                               c(nrow(train_pos), nrow(train_un))))
    
    index <- createFoldsPu( train_pu_y, k=k, 
                            indepUn=indep_un, seed=seed )
    
    model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index)
    
    pred <- predict(un, model, returnRaster = FALSE)
    
    th <- thresholdNu(model, pred, expand=expand)
    
    pred_in_pred_neg <- which(pred_neg==0)
    new_neg_in_pred_neg <- pred_in_pred_neg[pred<th]
    pred_neg[ new_neg_in_pred_neg ] <- iter
    
    if (!is.null(test_set)) {
      pred_in_pred_neg_test <- which(pred_neg_test==0)
      ans <- predict(model,
                     test_set[pred_in_pred_neg_test, -1])
      pred_test <- rep(1+min(ans), nrow(test_set))
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
    pdf(fname(folder_out, iter, ".pdf", "grid"))
    trellis.par.set(caretTheme())
    print(ans)
    dev.off()
    
    ### histogram
    pdf(fname(folder_out, iter, ".pdf", "histogram"))
    hist(model, pred, main=param_as_str)
    abline(v=c(th, attr(th, "th_non_expanded")))
    dev.off()
    
    ### featurespace
    if (ncol(train_pu_x)==2) {
      pdf(fname(folder_out, iter, ".pdf", "featurespace"))
      featurespace(model, 
                   thresholds=c(th, attr(th, "th_non_expanded")), 
                   main=param_as_str)
      dev.off()
    }
     
    ### test
    if (ncol(train_pu_x)==2) {
      pdf(fname(folder_out, iter, ".pdf", "eval_pn"))
      plot(ev, main=paste("max. Kappa:", round(max(ev@kappa), 2)))
      dev.off()
    }
    
    ### change
    
    
    ### save results of this iteration
    save(iter, model, pred, th, pred_neg, pred_neg_test, ev, 
         seed_global, seed, 
         file=fname(folder_out, iter, ".RData", "results") )
    
    if (iter==iter_max)
      STOP=TRUE
    
    # update un
    un_bu <- un
    un <- rasterTiled(un$raster, 
                       mask=un$validCells[pred>=th], 
                       nPixelsPerTile = nPixelsPerTile)
  
  }
  
  return(un)

}
