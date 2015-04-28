#' @export 
get_ioccObj <- function(base_dir, iter, 
                        fname_U=NULL, U_as_df = FALSE,
                        status="beforeNegClassification", 
                        modRow=NULL, test_set=NULL,
                        subfolder_test_set=NULL,
                        th=NULL,
                        verbose=FALSE) { 
  
  if (verbose)
    cat("LOADING: ", get_fname("ini", base_dir, iter=0), "\n")
  load(iocc_filename(base_dir, 0, "_ini.RData"))
  
  if (verbose)
    cat("LOADING: ", get_fname("results", base_dir, iter=0), "\n")
  load(get_fname("model", base_dir, iter=iter))
  load(get_fname("predictions", base_dir, iter=iter))
  load(get_fname("results", base_dir, iter=iter))
  
  dir.create(iocc_filename(base_dir, iter, "/predU"),
             showWarnings=FALSE, recursive=TRUE)
  
  if (status=="beforeNegClassification") {
    new_neg_in_pred_neg <- pred_neg==iter
    pred_neg[new_neg_in_pred_neg] <- 0
    if (exists("pred_neg_test"))
      pred_neg_test[pred_neg_test==iter] <- 0
  }
  
  # MIGHT CAUS A BUG IF fname_U moves (overwritten by fname_U in ini.RData!
  if (!is.null(fname_U)) {
    idx <- which(pred_neg==0)
    if (verbose)
      cat("CREATING rasterTiled OBJECT: ", fname_U, "\n")
    U <- rasterTiled(brick(fname_U), idx)
    if (U_as_df) {
      U_df <- extract_bandwise(fname_U, idx)
    } else {
      U_df=NULL
    }
  } else {
    U <- rasterTiled(brick(fname_U), idx)
  }
  
  if (!is.null(modRow)) {
    fname_pred <- iocc_filename(base_dir, iter, 
                                "/predU/model-", 
                                formatC(modRow, width=3, 
                                        flag=0), ".RData")
    fname_pred_img <- gsub("RData", "tif", fname_pred)
    fname_hist <- gsub(".RData", "_histogram.pdf", fname_pred)
    fname_histUth <- gsub(".RData", "_histogramUths.pdf", fname_pred)
    # if (modelPosition(model)$row != modRow) {
    if (verbose)
      cat("UPDATING MODEL TO MODEL #", modRow, "\n")
    model <- update(model, modRow=modRow)
    
    if (verbose)
      cat("Looking for predictions in ", fname_pred, "\n",
          "Exists? - ", file.exists(fname_pred), "\n")
    if (!file.exists(fname_pred)) {
      if (verbose)
        cat("\nPREDICTING (may take a while).")
      pred = predict(U, model, returnRaster=FALSE)
      save(pred, file=fname_pred)
    } else {
      if (verbose)
        cat("LOADING PREDICTIONS FROM: ", fname_pred, "\n")
      load(fname_pred)
    }
    if (!file.exists(fname_pred_img)) {
      if (verbose)
        cat("\nSaving predictions as raster: ", 
            gsub("RData", "tif", fname_pred), "\n")
      write_rasterTiled(U, pred, fname_pred_img)
    }
    
    #     if (!is.null(test_set) & exists("pred_neg_test")) {
    #       pred_test <- ev <- NULL  # before with pred_neg_test <- NULL and two lines before...
    #       if (!exists("th")) {
    #         if (verbose)
    #           cat("SETTING THRESHOLD TO 0.", "\n")
    #         th = 0
    #       }
    #       dummy <- .evaluate_iocc(model, test_set, 
    #                               pred_neg_test, th, iter)
    #       pred_test <- dummy$pred_test
    #       pred_neg_test <- dummy$pred_neg_test
    #       ev <- dummy$ev
    #     }
    #     save(pred, pred_test, pred_neg_test, ev, file=fname_pred)
    
  pred_test <- pred_neg_test <- ev <- NULL
  
  ev <- evaluate_iocc(baseDir=baseDir, iters=iter, 
                      refset=test_set, modRows=modRow, 
                        subfolder=subfolder_test_set)
  if (is.list(ev))
      ev <- ev[[1]]
  
  tdp <- th_dep_performance(settings$baseDir, iter,
                            modRows=modRow, folder_U="PNL", 
                            return_val=TRUE, plot=FALSE)
    
  ### histogram
  ioccObj <- .c_ioccObj(environment())
  plot_iocc(ioccObj, what="hist", 
            outfile=gsub(".RData", "_histogram.pdf", fname_pred))
  plot_iocc(ioccObj, what="featurespace", 
            outfile=gsub(".RData", "_featurespace.pdf", fname_pred))
  require(threshold)
  
  set.seed(seed)
  index_sub <- sort(sample(length(pred), min(10000, length(pred))))
  ths <- find_threshold(pred[index_sub], method=c("entropy", "nmm"))
  
  scaleToYLimits <- function(y, yLimits) {
    approx(c(0,1), yLimits, y)$y
  }
  
  pdf(fname_histUth)
  yli <- plot_histUperf(model, pred[index_sub], tdp$perf, axis=FALSE)
  lines(ev@t, scaleToYLimits(ev@TPR, yli), lwd=1, lty=4) # PA
  lines(ev@t, scaleToYLimits(ev@PPP, yli), lwd=1, lty=4) # UA
  lines(ev@t, scaleToYLimits(ev@kappa, yli), lwd=1, lty=1) # PA
  plot(ths$nmm, add=TRUE)
  ths_add=list( "dummy"=c(tdp$perf$th[which.max(tdp$perf$puF)]))
  names(ths_add) <- paste0('puF:', signif(max(tdp$perf$puF), 3))
  plot(ths, ths_add=ths_add, add=TRUE)
  lines(ev@t, ev@kappa, lwd=2)
  dev.off()
  
  fname_pred_img_bin_nmm <- gsub(".tif", "_bin-nmm.tif", fname_pred_img)
  fname_pred_img_bin_nmm_sieved <- gsub(".tif", "_bin-nmm_sieved.tif", fname_pred_img)
  write_rasterTiled(U, pred>ths$nmm, fname_pred_img_bin_nmm, 
                    nodata=0, overwrite=TRUE)
  pred_bin_nmm_sieved <- gdal_sieve(srcfile = fname_pred_img_bin_nmm,
                         dstfile = fname_pred_img_bin_nmm_sieved, 
                         st = sieve, returnRaster=TRUE)[][U$validCells]  
  
  
  #         pdf(gsub(".RData", "_hist.pdf", fname_pred))
  #         hist(model, pred, main=param_as_str)
  #         # try(abline(v=c(th, attr(th, "th_non_expanded"))))
  #         if (!is.null(test_set)) {
  #           rug(pred_test[test_set$y==1], ticksize = 0.03, col=colors_pn$p)
  #           rug(pred_test[test_set$y==-1], ticksize = -0.03, col=colors_pn$n)
  #         }
  #         dev.off()
}
#     } else {
#       if (verbose)
#         cat("KEEPING MODEL", "\n")
#       ioccObj <- .c_ioccObj(environment())
#       if (!file.exists(fname_pred_img))
#         write_rasterTiled(U, pred, fname_pred_img, nodata=min(pred))
#       if (!file.exists(fname_pred_img))
#         plot_iocc(ioccObj, what="hist", outfile=fname_hist)
#     }
# }

file=iocc_filename(base_dir, iter, "_results.RData")
n_un_iter <- sum(pred_neg==0)
baseDir <- base_dir
ioccObj <- .c_ioccObj(environment())

#   .c_ioccObj <- function(...) {
#     n_un_iter <- sum(pred_neg==0)
#     namesList <- ls()
#     objList <- list()
#     for (i in 1:length(namesList))
#       objList[[i]] <- eval(parse(text=namesList[i]))
#     names(objList) <- namesList
#     objList <- structure(objList, class="iocc")
#     return(objList)
#   }
invisible(ioccObj)
}