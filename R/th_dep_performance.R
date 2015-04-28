#' @export
th_dep_performance <- function(base_dir, iter, modRows=NULL,
                               folder_U=NULL, 
                               rm_pos=NULL,
                               return_val=FALSE,
                               overwrite=FALSE,
                               plot=TRUE, 
                               model=NULL) {
  #' @param return_val "None"
     
     .th_dep_performance <- function(model, base_dir, iter, modRow,
                                     folder_U, make_plot=FALSE,
                                     return_val=FALSE) {
       
       model <- update(model, modRow=modRow)
       fname_data <- get_fname("perf_data", base_dir, iter=iter, 
                               modRow=modRow, subfolder=folder_U)
       fname_hist <- get_fname("perf_hist", base_dir, iter=iter, 
                               modRow=modRow, subfolder=folder_U)
       
       .plot_hist <- function(model, hop, perf) {
         h <- hist(model, hop$un)
         y <- c(0, h$ylim[2])
         scaleMetric <- function ( x, y )
           approx(range(x), y, xou=x)$y
         lines(perf$th, scaleMetric(perf$puF, y), lwd=2)
         lines(perf$th, scaleMetric(perf$puF1, y), lwd=2, lty=2)
         axis(4, at=y, labels=round(range(perf$puF), 2))
         axis(4, at=y, labels=round(range(perf$puF1), 2), lty="dashed", 
              line=-.5, tick=-1, padj=-4)
       }
       
       if (!file.exists(fname_data) | overwrite) {
         hop <- get_puSet(base_dir, iter, modRow=modRow,
                          folder_U=folder_U, verbose=TRUE,
                          model=model)
         if (!is.null(rm_pos)) {
           hop$pos <- hop$pos[!(as.numeric(names(hop$pos)) %in% rm_pos)]
         }
         
         # hop$pos <- hop$pos[!(hop$pos %in% bp_pos$out)]
         dfs <- dataForSummaryFunction(hop)
         
         # Exclude numerically instable regions
         # bp_pos <- boxplot(hop$pos, plot=FALSE)
         # th_upper <- bp_pos$stats[4]
         th_upper <- median(hop$pos)
         th_lower <- min(hop$pos)
         n_th <- 50
         inRange_pos <- sum(hop$pos>=th_lower & hop$pos<=th_upper)
         if (inRange_pos>n_th) {
           thresholds <- seq(th_lower, th_upper,
                             length.out=n_th)[2:(n_th-1)]
         } else {
          pos <- sort(hop$pos[hop$pos<=median(hop$pos)])
          pos_halfDiff <- diff(pos)/2
          thresholds <- pos[1:(length(pos)-1)] + (pos_halfDiff/2)
          unSTminPos <- hop$un<min(pos)
          if (any(unSTminPos)) {
            thresholds <- c(mean(max(hop$un[unSTminPos], na.rm=TRUE), min(pos)), thresholds)
          } else {
            thresholds <- c(pos[1]-pos_halfDiff[1], thresholds)
          }
         }
         # add 0
         thresholds <- c(thresholds[thresholds<0], 0, 
                         thresholds[thresholds>0])
           
         perf <- puSummaryThLoop(dfs, thresholds=thresholds,
                                 returnAll=TRUE)
         defautPuSum <- puSummary(dfs)
         
         save(hop, perf, defautPuSum, file=fname_data)
         
         if (!file.exists(fname_hist) | overwrite) {
           pdf(fname_hist)
           .plot_hist(model, hop, perf)
           dev.off()
           if (plot)
             .plot_hist(model, hop, perf)
         }
         
         if (ncol(model$trainingData)==3) {
           fname_fs <- gsub("histogram", "featurespace", fname_hist)
           if (!file.exists(fname_fs) | overwrite) {
             pdf(fname_fs)
             featurespace(model)
             dev.off()
             if (plot)
               featurespace(model)
           }
         }
         
         if (make_plot)
           .plot_hist(model, hop, perf)
         
       } else if (file.exists(fname_data) & (return_val)) {
         load(fname_data)
         if (make_plot)
           model <- get_model(base_dir=base_dir, iter=iter, 
                              modRow=modRow, verbose=FALSE)
         .plot_hist(model, hop, perf)
       }
       if (return_val) {
         return(list(hop=hop, 
                     perf=perf,
                     defautPuSum=defautPuSum))
       }
     }
     
     if (is.null(modRows))
       if (is.null(model)) {
         modRows <- 1:nrow(get_model(base_dir=base_dir, iter=iter)$results)
       } else {
         modRows <- 1:nrow(model)
       }
     outdir_data <- get_fname("perf_data", base_dir, iter=iter, 
                              modRow=modRows[1], subfolder=folder_U,
                              dirOnly=TRUE)
     
     if (length(modRows)==1) {
       return_val=TRUE
     }
     if (!return_val) {
       x_pred_list <- dir(outdir_data)[grep("hopUperf", dir(outdir_data))]
       available <- sapply(x_pred_list, function(x) as.numeric(substr(x, 1, 3)))
       modRows <- modRows[!(modRows %in% available)]
     }
     
     if ((length(modRows) == 0) & (!return_val)) {
       cat("All predictions already computed.\n")
       return(NULL)
     }
     
     if (is.null(model))
       model <- get_model(base_dir=base_dir, iter=iter)
         
     cat("Processing ", length(modRows), " models.\n")
     
     if (length(modRows) > 1) {
       rtrn_list <- 
         foreach(modRow = modRows, 
                 .packages=c("oneClass", "iterOneClass", "kernlab"), 
                 .verbose=FALSE) %dopar% .th_dep_performance(model, 
                   base_dir, iter, modRow, folder_U, return_val=TRUE)
     } else if (length(modRows)==1) {
       rtrn_list <- .th_dep_performance(model, base_dir, iter, modRows, folder_U, 
                                        return_val=TRUE, make_plot=TRUE)
     }
     invisible(rtrn_list)
}