#'@export
plot_histUperf <- function(model, pred, perf, axis=TRUE) {
 h <- hist(model, pred)
 y <- c(0, h$ylim[2])
 scaleMetric <- function ( x, y )
   approx(range(x), y, xou=x)$y
 lines(perf$th, scaleMetric(perf$puF, y), lwd=2)
 # lines(perf$th, scaleMetric(perf$puF1, y), lwd=2, lty=2)
 if (axis)
   axis(4, at=y, labels=round(range(perf$puF), 2))
 text(x=min(pred)+abs(diff(range(pred)))*.1, y=y[2], 
      labels=paste0("max.puF:", signif(max(tdp$perf$puF), 3)))
 #axis(4, at=y, labels=round(range(perf$puF1), 2), lty="dashed", 
 #      line=-.5, tick=-1, padj=-4)
 invisible(y)
}