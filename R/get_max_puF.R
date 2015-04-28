#' @export
get_max_puF <- function(baseDir, iters, modRow_list, 
                        folder_U, plot=FALSE, sort_df=TRUE) {
  
  if (length(iters) == 1 & !is.list(modRow_list))
    modRow_list <- list(modRow_list)
  
  max.puF <- data.frame()
  counter <- 0
  for (i in iters) {
    counter <- counter + 1
    tdp.i <- th_dep_performance(baseDir, iter=i, 
                                modRows=modRow_list[[counter]],
                                folder_U=folder_U, return_val=TRUE,
                                plot=plot)
    tmp <- sapply(tdp.i, function(tdp.m) 
      max(tdp.m$perf$puF))
    tmp <- data.frame(iter=rep(i, length(tmp)), max.puF=tmp,
                      modRow=modRow_list[[counter]])
    max.puF <- rbind(max.puF, tmp)
  }
  if (sort_df)
    max.puF <- max.puF[sort(max.puF$max.puF, decreasing=TRUE, index.return=TRUE)$ix, ]
  
  row.names(max.puF) <- 1:nrow(max.puF)
  return(max.puF)
}