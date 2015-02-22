#'@export
prediction_image_filename <- function(base_dir, iter, modRow) {
  fname <- paste0(iocc_filename(base_dir=sttngs$baseDir,
                                      iter=iter), "/predU", 
                        "/model-", sprintf("%03d", modRow), 
                        ".tif")
  return(fname)
}
