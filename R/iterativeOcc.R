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
                          overwrite=FALSE, verbosity=3,
                          smallIniGrid=TRUE,
                          tuneGrid=expand.grid(sigma=1, 
                                               cNeg=2^seq(-10, 15, 3),
                                               cMultiplier=2^seq(2, 11, 2)),
                          useSigest=TRUE,
                          cvPu = TRUE,
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
  
  if (exists_fname("ini", settings$baseDir) & !overwrite) {  
    # LOAD ini, but not the baseDir
    basedir_bak <- settings$baseDir
    load(get_fname("ini", settings$baseDir))
    settings$baseDir <- basedir_bak
  }
  
  settings$colors_pu<- .get_colors("PU")
  settings$colors_pn <- .get_colors("PN")
  
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
  pred_rm_sum <- list()
  
  if (!is.null(settings$seed)) {
    seed <- settings$seed
  } else {
    seed <- round(runif(1, 1, 1000000), 0)
  }
  
  dir.create(settings$baseDir, showWarnings=FALSE)
  
  pred_neg <- vector(mode="integer", length=n_un)
  
  validCells <- U$validCells
  fname_U <- U$raster@file@name
  
  tuneGrid.bak <- tuneGrid
  
  save(fname_U, n_un, nPixelsPerTile, validCells, seed, 
       settings, PN, tuneGrid, smallIniGrid, useSigest,
       file=get_fname("ini", settings$baseDir))  # SAVE ini
  
  attach(settings)
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ### iterate 
  iter = 0
  STOP = FALSE
  
  time_stopper <- c()
  time_stopper_model <- time_stopper_predict <- 
    time_stopper_sieve <- time_stopper_writeTiles <- c()
  th_all <- c()
  while(!STOP) {  # start the iterations
    iter <- iter + 1
    seed_iter <- seed*iter
    if (!file.exists(iocc_filename(baseDir, iter, "_results.RData"))) {
      
      time_stopper <- rbind(time_stopper, proc.time())
      
      n_un_iter <- length( U$validCells )
      
      cat(sprintf("Iteration: %d \n\tPercent unlabeled: %2.2f", iter, (n_un_iter/n_un)*100 ), "\n")
      
      if (!file.exists(get_fname("model", baseDir, iter=iter))) {
        ### classification
        cat("\tTraining ... ")
        ans <- proc.time()
        train_un <- sample_rasterTiled(U, size=nTrainUn, seed=seed_iter)
        train_pu_x <- rbind(P, train_un)
        train_pu_y <- puFactor(rep(c(1, 0), 
                                   c(nrow(P), nrow(train_un))))
        
        # tuneGrid
        if (useSigest) {
          set.seed(seed_iter)
          sigma <- sigest(train_pu_x)
        } else {
          sigma <- unique(tuneGrid[, "sigma"])
        }
        cNeg <- unique(tuneGrid[, "cNeg"]) # 2^seq(-10, 15, 3)
        cMultiplier <- unique(tuneGrid[, "cMultiplier"]) # 
        if (iter==1) {
          if (smallIniGrid) {
            set.seed(seed_iter)
            sigma <- max(sigest(train_pu_x))
            cMultiplier=1
          }
        }
        tuneGrid = expand.grid(sigma=sigma, 
                               cNeg=cNeg,
                               cMultiplier=cMultiplier)
        if (!is.na(indepUn)) {
          index <- createFoldsPu( train_pu_y, k=kFolds,
                                  indepUn=indepUn, seed=seed_iter)
        } else {
          set.seed(seed_iter)
          index <- createFolds(train_pu_y, k=kFolds)
        }
        model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index, 
                          tuneGrid=tuneGrid, ...)
        tuneGrid <- tuneGrid.bak
        
        if (!is.null(use_pnpAt)) {
          cat("Threshold at ", use_pnpAt, " ... ")
          # if (oneClass:::.foreach.exists()) { # ASSUMED
          newMetrics <- foreach(mm=1:nrow(model$results),
                                .combine=rbind,
                                .packages="oneClass") %dopar%
            oneClass::pnpAt(model, modRow=mm)
          
          model$results <- cbind(model$results, newMetrics)
          
          if (all(model$results[, use_pnpAt]==1)) {
            model <- update(model, modRank=1, metric="sDff")
          } else {
            model <- update(model, modRank=1, metric=use_pnpAt)
          }
          #hist(model)
          #featurespace(model, thresholds=newMetrics[idx, 1])
        }
        
        fnameTab <- iocc_filename(baseDir, iter, "_modsel_table.txt")
        capture.output(print(signif(model$results, 2),
                             print.gap=3), file=fnameTab)
        
        dir.create(iocc_filename(baseDir, iter, "/modsel"), recursive=TRUE)
        for (mr in 1:nrow(newMetrics)) {
          fname_mr <- iocc_filename(baseDir, iter, "/modsel/", sprintf("%03d", mr), "_hist.pdf")
          anstry <- try({
            pdf(fname_mr)
            hist(update(model, modRow=mr), th=newMetrics[mr, gsub("pnp", "th", use_pnpAt)])
            dev.off()
            anstry <- NULL
          })
        }
        dummy <- signif(newMetrics, 2)
        dummy <- cbind(dummy, sel="") # $sel = ""
        dummy[modelPosition(model)$row, "sel"] = "*" # $sel[modelPosition(model)$row] = "*"
        sink(iocc_filename(baseDir, iter, "/modsel/newMetrics.txt"))
        print(dummy)
        sink()
        
        time_stopper_model <- rbind(time_stopper_model, (proc.time()-ans)[1:3])
        save(model, time_stopper_model, file=get_fname("model", baseDir, iter=iter))
      } else {
        load(get_fname("model", baseDir, iter=iter))
      }
      cat("Completed in", time_stopper_model[iter, 3], "sec.\n")
      
      cat("\tPrediction ... ")
      
      fname_pred <- get_fname("predictions", baseDir, iter=iter)
      if (!file.exists(fname_pred)) {
        
        ans <- proc.time()
        pred <- predict(U, model, returnRaster = FALSE, 
                        fnameLog = iocc_filename(baseDir, iter, 
                                                 "_log_prediction.txt"))
        time_stopper_predict <- rbind(time_stopper_predict, (proc.time()-ans)[1:3])
        save(pred, time_stopper_predict, file=get_fname("predictions", baseDir, iter=iter))
      } else {
        load(file=fname_pred)
      }
      cat("Completed in", time_stopper_predict[iter, 3], "sec.\n")
      
      th <- thresholdLikN(model, pred, thStart=thStartLikN, expand=expand)
      th_all <- c(th_all, th)
      
      pred_rm <- pred<th
      pred_rm_bak <- pred_rm
      pred_rm_sum[[iter]] <- c(th=sum(pred_rm), sieve=0)
      
      if (!is.null(sieve)){
        ans <- proc.time()
        fname_pred_r <- gsub(".RData", ".tif", fname_pred)
        fname_pred_rs <- gsub(".RData", "_sieved.tif", fname_pred)
        
        write_rasterTiled(U, pred_rm, # attr(th, "th_non_expanded"), 
                          fname_pred_r, nodata=TRUE, 
                          datatype="INT1U", overwrite=TRUE)
        r_sieved <- gdal_sieve(srcfile = fname_pred_r,
                               dstfile = fname_pred_rs, 
                               st = sieve, returnRaster=TRUE)
        pred_rm <- r_sieved[][U$validCells]==1
        pred_rm_sum[[iter]]["sieve"] <- sum(pred_rm)
        time_stopper_sieve <- rbind(time_stopper_sieve, (proc.time()-ans)[1:3])
        cat("Completed in", time_stopper_sieve[iter, 3], "sec.\n")
      }
      
      pred_in_pred_neg <- which(pred_neg==0)
      new_neg_in_pred_neg <- pred_in_pred_neg[pred_rm]
      pred_neg[ new_neg_in_pred_neg ] <- iter
      
      n_un_all_iters[iter+1] <- n_un_all_iters[iter]-length(new_neg_in_pred_neg)
      
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
      # update U
      ans <- proc.time()
      U <- rasterTiled(U$raster, 
                       mask=U$validCells[!pred_rm], 
                       nPixelsPerTile=nPixelsPerTile,
                       asRData=TRUE)
      time_stopper_writeTiles <- rbind(time_stopper_writeTiles, (proc.time()-ans)[1:3])
      
      ### time
      # time_stopper <- rbind(time_stopper, proc.time())
      time_stopper[iter, ] <- (proc.time()-time_stopper[iter, ])
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
      # Test
      if (!is.null(PN)) {
        cat("\tEvaluation ... ")
        dummy <- .evaluate_iocc(model, PN, pred_neg_test, th, iter)
        pred_test <- dummy$pred_test
        pred_neg_test <- dummy$pred_neg_test
        ev <- dummy$ev
      } else {
        pred_test <- pred_neg_test <- ev <- NULL
      }
      
      ### save results of this iteration
      save(iter, th, th_all, pred_neg, # model, pred
           pred_rm_sum,
           pred_test, pred_neg_test, ev, 
           n_un_all_iters, seed, seed_iter,
           time_stopper, time_stopper_model, time_stopper_predict,
           file=iocc_filename(baseDir, iter, "_results.RData"))
      
      
      ### plot diagnostics
      cat("\n\tWriting results in folder ", baseDir)
      
      param_as_str <- .get_param_as_str(model)
      
      ioccObj <- .c_ioccObj(environment())
      
      plot_iocc(ioccObj, what="grid", 
                outfile=iocc_filename(baseDir, iter, "_grid.pdf"))
      plot_iocc(ioccObj, what="hist", 
                outfile=iocc_filename(baseDir, iter, "_histogram.pdf"))
      plot_iocc(ioccObj, what="featurespace", 
                outfile=iocc_filename(baseDir, iter, "_featurespace.pdf"))
      plot_iocc(ioccObj, what="eval", 
                outfile=iocc_filename(baseDir, iter, "_eval_pn.pdf"))
      plot_iocc(ioccObj, what="n_unlabeled", 
                outfile=iocc_filename(baseDir, 0, "_n_unlabeled.pdf"))
      plot_iocc(ioccObj, what="time", 
                outfile=iocc_filename(baseDir, 0, "_time.pdf"))
      
      loaded = FALSE
    } else {
      load(iocc_filename(baseDir, iter, "_results.RData"))
      loaded = TRUE
    }
    if (is.character(iterMax)) {
      # iter of no change
      dff <- abs(diff(n_un_all_iters))
      if (length(grep("[+]", iterMax))==1) {
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
      rm(model, pred, th, ev, seed, seed_iter, 
         n_un_iter, train_un,
         train_pu_x, train_pu_y)
    }
    if (loaded) {
      cat(sprintf("\t%2.2f real and %2.2f CPU time required (results and time loaded saved results).",
                  time_stopper[iter, , drop=FALSE][,"elapsed"],
                  time_stopper[iter, , drop=FALSE][,"sys.self"]
      ), "\n")
      if (STOP) {
        load(get_fname("model", baseDir, iter=iter))
        load(get_fname("predictions", baseDir, iter=iter))
      }
    } else {
      cat(sprintf("\t%2.2f real and %2.2f CPU time required.",
                  time_stopper[iter, , drop=FALSE][,"elapsed"],
                  time_stopper[iter, , drop=FALSE][,"sys.self"]
      ), "\n")
    }
  }
  gc()
  return(
    structure(list(U = U,
                   iter=iter, 
                   model=model, 
                   pred=pred, 
                   th=th, 
                   th_all=th_all,
                   pred_neg=pred_neg, 
                   pred_rm_sum=pred_rm_sum, 
                   n_un_all_iters=n_un_all_iters,
                   pred_neg_test=pred_neg_test, 
                   ev=ev, 
                   seed=seed, 
                   seed_iter=seed_iter,
                   time_stopper=time_stopper, 
                   time_stopper_model=time_stopper_model, 
                   time_stopper_predict=time_stopper_predict,
                   time_stopper_writeTiles=time_stopper_writeTiles,
                   time_stopper_sieve=time_stopper_sieve,
                   baseDir=baseDir,
                   file=iocc_filename(baseDir, iter, "_results.RData")
    ), class = "iocc")
  )
}