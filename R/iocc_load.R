#' @export 
iocc_load <- function(base_dir, iter, 
                      filename_U=NULL, U_as_df = FALSE,
                      status="beforeNegClassification", 
                      modRow=NULL, test_set=NULL, th=NULL) { 
  warning("DEPRECTION WARNING. BETTTER USE 'get_ioccObj'.")
  cat("LOADING: ", iocc_filename(base_dir, 0, 
                                   "_ini.RData"), "\n")
  load(iocc_filename(base_dir, 0, "_ini.RData"))
  
  cat("LOADING: ", iocc_filename(base_dir, iter, 
                                   "_results.RData"), "\n")
  load(iocc_filename(base_dir, iter, "_results.RData"))

  dir.create(iocc_filename(base_dir, iter, "/predU"),
             showWarnings=FALSE)
  
  if (status=="beforeNegClassification") {
    pred_neg[pred_neg==iter] <- 0
    if (exists("pred_neg_test"))
      pred_neg_test[pred_neg_test==iter] <- 0
  }
  
  if (!is.null(filename_U)) {
    idx <- which(pred_neg==0)
    cat("CREATING rasterTiled OBJECT: ", filename_U, "\n")
    U <- rasterTiled(brick(filename_U), idx)
    if (U_as_df) {
      U_df <- extract_bandwise(filename_U, idx)
    } else {
      U_df=NULL
    }
  }
  
  if (!is.null(modRow)) {
    if (modelPosition(model)$row != modRow) {
      cat("UPDATING MODEL TO MODEL #", modRow, "\n")
      model <- update(model, modRow=modRow)
      fname_pred <- iocc_filename(base_dir, iter, 
                                  "/predU/model-", 
                                  formatC(modRow, width=3, 
                                          flag=0), ".RData")
      
      cat("Looking for predictions in ", fname_pred, "\n")
      cat("Exists? - ", file.exists(fname_pred), "\n")
      if (file.exists(fname_pred)) {
        cat("LOADING PREDICTIONS FROM: ", fname_pred, "\n")
        load(fname_pred)
      } else {
        cat("\nPREDICTING (may take a while).")
        pred = predict(U, model, returnRaster=TRUE)

        cat("\nSaving predictions as raster: ", 
            gsub("RData", "tif", fname_pred), "\n")
        writeRaster(pred, filename = gsub("RData", "tif", fname_pred), 
                    format="GTiff", overwrite=TRUE)
        pred = pred[][U$validCells]
        
        pred_test <- pred_neg_test <- ev <- NULL
        if (!is.null(test_set) & exists("pred_neg_test")) {
          if (!exists("th")) {
            cat("SETTING THRESHOLD TO 0.", "\n")
            th = 0
          }
          dummy <- .evaluate_iocc(model, test_set, 
                                  pred_neg_test, th, iter)
          pred_test <- dummy$pred_test
          pred_neg_test <- dummy$pred_neg_test
          ev <- dummy$ev
        }
        save(pred, pred_test, pred_neg_test, ev, file=fname_pred)
        
        ### histogram
        param_as_str <- paste(colnames(signif(model$bestTune)),
                              model$bestTune, sep=": ",
                              collapse=" | ")        
        colors_pu<- list(pos='#2166ac', un='#e0e0e0') 
        colors_pn <- list(pos='#2166ac', neg='#f4a582')
        
        pdf(gsub(".RData", "_hist.pdf", fname_pred))
        hist(model, pred, main=param_as_str)
        # try(abline(v=c(th, attr(th, "th_non_expanded"))))
        if (!is.null(test_set)) {
          rug(pred_test[test_set$y==1], ticksize = 0.03, col=colors_pn$p)
          rug(pred_test[test_set$y==-1], ticksize = -0.03, col=colors_pn$n)
        }
        dev.off()
        
      }
    } else {
      cat("KEEPING MODEL", "\n") 
    }
  }
  
  n_un_iter <- sum(pred_neg==0)
  namesList <- ls()
  objList <- list()
  for (i in 1:length(namesList))
    objList[[i]] <- eval(parse(text=namesList[i]))
  names(objList) <- namesList
  
  return(objList)
}