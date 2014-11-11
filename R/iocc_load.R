#' @export 
iocc_load <- function(base_dir, iter, 
                      filename_U=NULL, U_as_df = TRUE,
                      status="beforeNegClassification") { 
  
  load(iocc_filename(base_dir, 0, "_ini.RData"))
  load(iocc_filename(base_dir, iter, "_results.RData"))
  
  if (status=="beforeNegClassification") {
    pred_neg[pred_neg==iter] <- 0
    if (exists("pred_neg_test"))
      pred_neg_test[pred_neg_test==iter] <- 0
  }
  
  if (!is.null(filename_U)) {
    if (U_as_df) {
      U <- extract_bandwise(filename_U, which(pred_neg_test==0))
    } else {
      error("Only U_as_df is implemented yet.")
    }
  }
  
  namesList <- ls()
  objList <- list()
  for (i in 1:length(namesList))
    objList[[i]] <- eval(parse(text=namesList[i]))
  names(objList) <- namesList
  
  return(objList)
}