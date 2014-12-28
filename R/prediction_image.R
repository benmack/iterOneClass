#'@export
prediction_image_filename <- function(base_dir, iter, modRow) {
  fname <- paste0(iocc_filename(base_dir=sttngs$baseDir,
                                      iter=iter), "/predU", 
                        "/model-", sprintf("%03d", modRow), 
                        ".tif")
  return(fname)
}

#'@export
predict.iOcc <- function(sttngs, iter, modRow, 
                             filename_U=filename_U) {
  # get the data
  iocc.i <- iocc_load(sttngs$baseDir, iter, 
                      filename_U=filename_U, U_as_df=FALSE)
  model <- update(iocc.i$model, modRow=modRow)
  fname_res <- prediction_image_filename(sttngs$baseDir, iter, modRow)
  
  if (!file.exists(fname_res)) {
    dir.create(paste0(sttngs$baseDir, "/iter-", iter, 
                      "/predU"), recursive=TRUE)
    pred <- predict(model, iocc.i$U)
    save(pred, file = gsub("tif", "RData", fname_res))
    writeRaster(pred, filename = fname_res, format="GTiff")
  } else {
    pred <- raster(fname_res)
  }
  pred <- pred[][iocc.i$U$validCells]
  
  pdf(gsub(".tif", "_hist.pdf", fname_res))
    hist(model, pred)
  dev.off()

  return(list(model=model, pred=pred, fname_res=fname_res))
}
