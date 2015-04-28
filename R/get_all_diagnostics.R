# #' @export
# get_all_diagnostics <- function(iocc, iter, unSub=5000, 
#                                 modRows=NULL, seed=NULL, 
#                                 fname_image=NULL,
#                                 folder_suffix=NULL) {
# 
#   # REFACTOR: ALSO IN plot_iocc
#   # - ADDED:
#   modRow = NULL
#   # - |
#   if (is.character(iocc)) {
#     iocc <- get_ioccObj(iocc, iter, modRow=modRow)
#     baseDir <- iocc$baseDir
#     if (is.null(iter))
#       iter <- .nIter(baseDir)
#   } else {
#     baseDir = iocc$baseDir
#     if (is.null(iter)) {
#       iter <- iocc$iter
#     } else {
#       if (iter != iocc$iter)
#         iocc <- get_ioccObj(iocc$baseDir, iter, modRow=modRow)
#     }
#   }
#   
#   if (length(unSub) == 1) {
#     nUnSub <- unSub
#   } else {
#     nUnSub <- nrow(unSub)
#   }
#   
# #   if (is.null(fname_image))
# #     fname_image <- iocc$U$raster@file@name
#   if (is.null(folder_suffix)) {
#     folder_suffix <- paste0("predU-", nUnSub, "_s", seed)
#   
#   outdir <- get_fname("predU_U", iter, folder_suffix,
#                       dirOnly=TRUE)
#   outdir_fig <- get_fname("perf_hist", iter, folder_suffix,
#                           dirOnly=TRUE)
#   
# 
#   # dir.create(outdir, recursive = TRUE, showWarnings=FALSE)
# 
#   iocc.i <- get_ioccObj(baseDir, iter)  
#   # , filename_U=fname_image, U_as_df=FALSE  # U_as_df=TRUE for building hist with all unlabeled samples
#   if (is.null(modRows)) {
#     modRows <- 1:nrow(iocc.i$model$results)
#   }
#   # check which ones are already available
#   x_pred_list <- dir(outdir)[grep("_pred.", dir(outdir))]
#   available <- sort(sapply(x_pred_list, 
#                            function(x) as.numeric(substr(x, 1, 3))))
#   modRows <- modRows[!(modRows %in% available)]
#   
#   if (length(modRows) == 0) {
#     cat("All predictions already computed.\n")
#     return(NULL)
#   }
#   
#   ### --------------------------------------------------------
#   ### get histogram plots of all models 
#   # outdir <- paste0(iocc_filename(base_dir=baseDir, 
#   #                                iter=iter), "/predU-", unSub, "/")
#   # dir.create(outdir, recursive = TRUE, showWarnings=FALSE)
#   
#   fname <- get_fname("predU_U", iter, folder_suffix)
# 
#   U_df <- get_Usub(baseDir, iter, unSub, seed, folder_suffix)
# 
#   ### --------------------------------------------------------
#   ### get histogram plots of all models 
#   predict_U <- function(iter, modRow, model, x_un, baseDir, folder_suffix) {
#     cat(modRow, "\n")
#     model <- update(model, modRow=modRow)
#     pred <- predict(model, x_un)
#     dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
#     save(pred, file=get_fname("predU_data", baseDir, iter, modRow,
#                       subfolder=folder_suffix))
#   }
#   
#   foreach(modRow = modRows, 
#           .packages=c("oneClass", "kernlab"), 
#           .verbose=TRUE) %dopar% predict_U(iter, modRow, iocc.i$model, 
#                                            U_df, baseDir, folder_suffix)
#   
# #   foreach(mm = modRows, 
# #           .packages=c("oneClass", "kernlab"), 
# #           .verbose=TRUE) %dopar% write_hist(iocc.i$model, 
# #                                             x_un=U_df,  # iocc.i$U_df
# #                                             modRows=mm, 
# #                                             thDepPerf=TRUE, 
# #                                             folder_out=outdir_fig,
# #                                             format='pdf')
# }