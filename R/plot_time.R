#' @export
plot_time <- function(ioccObj) {
  
  counts <- rbind(ioccObj$time_stopper_model[, "elapsed"], 
                  ioccObj$time_stopper_predict[, "elapsed"], 
                  ioccObj$time_stopper[, "elapsed"] - 
                  (ioccObj$time_stopper_model[, "elapsed"] + 
                   ioccObj$time_stopper_predict[, "elapsed"]))
  rownames(counts) = c("Training", "Prediction", "Other")
  barplot(counts, main="Elapsed time [s]",
    xlab="iteration", col=c("darkblue","red", "grey"),
    legend = rownames(counts))
  colnames(counts) = paste0("iter", 1:ncol(counts))
  return (counts)
}