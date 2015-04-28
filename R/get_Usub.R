#'@export
get_Usub <- function(baseDir, iter, unSub, seed, folder_U) {
  fname <- get_fname("predU_U", baseDir, iter=iter, 
                     subfolder=folder_U)
  if ( !file.exists(fname) ) {
    if (length(unSub) == 1) {
      iocc.iter <- get_ioccObj(baseDir, iter) 
      U_df <- sample_rasterTiled(iocc.iter$U, unSub, seed=seed)
    } else {
      U_df <- unSub
    }
    save(U_df, file=fname)
  } else {
    load(fname)
  }
  invisible(U_df)
}
