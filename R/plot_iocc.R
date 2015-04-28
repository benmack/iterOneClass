#' @name plot_iocc
#' @title Create different plots of the iterOcc outcome. 
#' @param iocc The base directory or an iocc object.
#' @param iter The iteration 
#' @param what The type of plot. 'grid', 'hist', 'featurespace', 
#' 'test', 'n_unlabeled', or 'time'
#' @export
plot_iocc <- function(iocc, iter=NULL, what="hist", 
                      modRow=NULL, pred=NULL, plotTh=TRUE,
                      outfile=NULL) {
  
  # REFACTOR: ALSO IN get_all_dagnostics
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

  colors_pn <- .get_colors("PN")
  colors_pu <- .get_colors("PU")

  param_as_str <- .get_param_as_str(iocc$model)
  # pdf(iocc_filename(baseDir, iter, "_histogram.pdf"))
  
  if (what == "grid") {
    ans <- try(plot(iocc$model, plotType="level", digits=1), silent=TRUE)
    if (class(ans) == "try-error")
      ans <- plot(iocc$model, digits=1)
    if (!is.null(outfile))
      pdf(outfile)
    trellis.par.set(caretTheme())
    print(ans)
    if (!is.null(outfile))
      dev.off()
    
  }
  rtrn_hist <- NULL
  if (what == "hist") {
    idx_rej_iter <- iocc$pred_neg>=iter
    iocc$pred_neg[idx_rej_iter] <- 0
    if (!is.null(outfile))
      pdf(outfile)
    if (!is.null(pred)) {
      rtrn_hist <- hist(iocc$model, pred, main="")
      strPredSub <- paste0(" | #Usub: ", length(pred))
    } else {
      rtrn_hist <- hist(iocc$model, iocc$pred, main="")
      strPredSub <- ""
    }
    title(main=paste(param_as_str, "\n", "iter ", iocc$iter, 
                     " | #U=", iocc$n_un_all_iters[iter], 
                     "(", round(iocc$n_un_all_iters[iter]/
                                  iocc$n_un_all_iters[1]*100, 2), "%)", strPredSub))
    if (plotTh)
      abline(v=c(iocc$th, attr(iocc$th, "th_non_expanded")))
    if (!is.null(iocc$PN)) {
      rug(iocc$pred_test[PN$y==1], ticksize = 0.03, col=colors_pn$p)
      rug(iocc$pred_test[iocc$PN$y==-1], ticksize = -0.03, col=colors_pn$n)
    }
    if (!is.null(outfile))
      dev.off()
  }
  if (what == "featurespace") {
    if (ncol(iocc$model$trainingData)==3) {
      if (!is.null(outfile))
        pdf(outfile)
      featurespace(iocc$model, 
                   thresholds=c(iocc$th, attr(iocc$th, "th_non_expanded")), 
                   main=param_as_str)
      if (!is.null(outfile))
        dev.off()    
    }
  }
  if (what == "eval") {
    if (!is.null(iocc$ev)) {
      if (!is.null(outfile))
        pdf(outfile)
      plot(iocc$ev, main=paste("max. Kappa:", round(max(iocc$ev@kappa), 2)))
      if (!is.null(outfile))
        dev.off()
    }
  }
  if (what == "n_unlabeled") {
    if (!is.null(outfile))
      pdf(outfile)
    dff <- abs(diff(iocc$n_un_all_iters))
    plot(1:(iter+1), iocc$n_un_all_iters, type="b", 
         xlab="iteration", ylab="# unlabeled", log="y")
    text(2:(iter+1), iocc$n_un_all_iters[2:(iter+1)], 
         label = paste("d:", dff), pos=c(3))   
    if (!is.null(outfile))
      dev.off()
  }
  if (what == "time") {
    if (!is.null(outfile))
      pdf(outfile)
    iocc_temp <- list(time_stopper_model=iocc$time_stopper_model[1:iter, , drop=FALSE],
                 time_stopper_predict=iocc$time_stopper_predict[1:iter, , drop=FALSE],
                 time_stopper=iocc$time_stopper[1:iter, , drop=FALSE])
    plot_time(iocc_temp)
    if (!is.null(outfile))
      dev.off()
  } 
  iocc$rtrn_hist <- rtrn_hist
  invisible(iocc)
}
