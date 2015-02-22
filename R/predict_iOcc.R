#'@export
predict_iOcc <- function(sttngs, iter, modRow, 
                             filename_U=filename_U) {
  # get the data
  iocc.i <- iocc_load(sttngs$baseDir, iter, 
                      filename_U=filename_U, U_as_df=FALSE)
  model <- update(iocc.i$model, modRow=modRow)
  fname_img <- prediction_image_filename(sttngs$baseDir, iter, modRow)
  fname_rd <- gsub("tif", "RData", fname_img)

  if (!file.exists(fname_rd)) {
    dir.create(paste0(sttngs$baseDir, "/iter-", iter, 
                      "/predU"), recursive = TRUE, 
               showWarnings=FALSE)
    pred <- predict(model, iocc.i$U)
    writeRaster(pred, filename = fname_img, format="GTiff")
    pred_img <- pred
    pred <- pred[][iocc.i$U$validCells]
    save(pred, file = gsub("tif", "RData", fname_img))
  } else {
    load(fname_rd)
  } 
  # pred <- pred[][iocc.i$U$validCells]
  
  pdf(gsub(".tif", "_hist.pdf", fname_img))
    hist(model, pred)
  dev.off()

  return(list(model=model, pred=pred, fname_img=fname_img))
}
