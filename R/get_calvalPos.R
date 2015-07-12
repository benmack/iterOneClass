#' @export
get_calvalPos <- function(oc, P, modRows) {
  calval <- list()
  for (mr in modRows) {
    cat(mr, ".")
    oc <- update(oc, modRow=mr)
    val <- holdOutPredictions(oc)$pos
    cal <- predict(oc, P[as.numeric(names(val)), ])
    #cal <- cal[sort(as.numeric(names(val)), index.return=T)$ix]
    calval <- c(calval, 
                list(list(cal=cal, 
                          val=val,
                          diff=val-cal)))
    }
  return(calval)
  }


# par(las=2)
# boxplot(lapply(calval, function(cv) 
#   cv$diff / diff(range(boxplot.stats(cv$diff)$stats))),
#   names=modRows_unique[[finalIter]])
# 
# boxplot(lapply(calval, function(cv) 
#   cv$diff / diff(range(c(
#     boxplot.stats(cv$cal)$stats,
#     boxplot.stats(cv$val)$stats)))),
#   names=modRows_unique[[finalIter]])
# 
# plot(sapply(calval, function(cv) cor(cv$cal, cv$val)), pch=NA)
# text(sapply(calval, function(cv) cor(cv$cal, cv$val)), 
#      as.character(modRows_unique[[finalIter]]))