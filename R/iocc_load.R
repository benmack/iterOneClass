#' @export 
iocc_load <- function(base_dir, iter, 
                      filename_U=NULL, U_as_df = FALSE,
                      status="beforeNegClassification", 
                      modRow=NULL, test_set=NULL, th=NULL) { 
  
  cat("LOADING: ", iocc_filename(base_dir, 0, 
                                   "_ini.RData"), "\n")
  load(iocc_filename(base_dir, 0, "_ini.RData"))
  
  cat("LOADING: ", iocc_filename(base_dir, iter, 
                                   "_results.RData"), "\n")
  load(iocc_filename(base_dir, iter, "_results.RData"))
  
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
      if (exists(fname_pred)) {
        cat("LOADING PREDICTIONS FROM: ", fname_pred, "\n")
        load(fname_pred)
      } else {
        cat("\nPREDICTING (may take a while).")
        pred = predict(U, model, returnRaster=FALSE)

        pred_test <- ev <- NULL
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
      }
    } else {
      cat("KEEPEING MODEL", "\n") 
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