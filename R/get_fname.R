#' @name get_fname
#' @title Get the filename of saved data 
#' @description \code{\link{iterativeOcc}} and other functions store intermediate and final results to disc. With this function the filename of specific data packages can be generated. 
#' @details The following values are possible:
#' \itemize{
#' \item ini}
#' @param what Filename of what kind of data.
#' @param baseDir the results' base directory
#' @param iter the iteration
#' @param modRow the model id/row/parameter setting 
#' @export
get_fname <- function(what, baseDir, iter=NULL, modRow=NULL,
                      subfolder=NULL, file_suffix, dirOnly=FALSE) {
  
  if (what == "ini") {
    dname <- baseDir
    fname <- paste0("iter-", 0, "_ini.RData")
    
  } else if (what == "results") {
    dname <- baseDir
    fname <- paste0("iter-", iter, "_results.RData")
    
  } else if (what == "predImg") {
    dname <- baseDir
    fname <- paste0("iter-", iter, "_predImg.tif")
    
    ### SUBFOLDERS
  } else if  (what == "unique_models") {
    if (is.null(subfolder)) {
      dname <- baseDir
      fname <- paste0("iter-", iter, "_unique_models.RData")
    } else {
      dname <- paste0(baseDir, "/iter-", iter, "/", subfolder, "/x_rdata")
      fname <- paste0("unique_models.RData")
    }
    
  } else if  (what == "model") {
    if (is.null(subfolder)) {
      dname <- baseDir
      fname <- paste0("iter-", iter, "_model.RData")
    } else {
      dname <- paste0(baseDir, "/iter-", iter, "/", subfolder, "/x_rdata")
      fname <- "model.RData"
    }

  } else if  (what == "predictions") {
      dname <- baseDir
      fname <- paste0("iter-", iter, "_predictions.RData")
  
  } else if  (what == "predU_U") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder, "/x_rdata"))
    fname <- paste0("U_df.RData")
    
  } else if  (what == "predU_data") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder, "/x_rdata"))
    fname <- paste0(sprintf("%03d", modRow), "_pred.RData")
    
  } else if  (what == "perf_data") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder, "/x_rdata"))
    fname <- paste0(sprintf("%03d", modRow), "_hopUperf.RData")
    
  } else if  (what == "perf_hist") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder))
    fname <- paste0(sprintf("%03d", modRow), "_histogram.pdf")

  } else if  (what == "threshs_uMod") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder, "/x_rdata"))
    fname <- paste0("threshs_uMod.RData")
  } else if  (what == "nmm_data_uMod") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder, "/x_rdata"))
    fname <- paste0("nmm_uMod.RData")
  } else if  (what == "nmm_plot") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder))
    fname <- paste0(sprintf("%03d", modRow), "_nmm.pdf")
    
    # EVALUATE
  } else if  (what == "eval") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder, "/x_rdata"))
    fname <- paste0(sprintf("%03d", modRow), "_ev.RData")
    
  } else if  (what == "eval_plot") {
    dname <- iocc_filename(baseDir, iter, "/",
                           paste0(subfolder))
    fname <- paste0(sprintf("%03d", modRow), "_eval.pdf")    
    
  } else {
    stop("No valid value for what.")
  }
  
  dir.create(dname, showWarnings=F, recursive=T)
  path <- paste0(dname, "/", fname)
  if (dirOnly) {
    return(dname)
  } else {
    return(path)
  }
}
