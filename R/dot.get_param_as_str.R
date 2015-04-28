#'@export
.get_param_as_str <- function(model) {
  param_as_str <- paste(colnames(model$bestTune), 
                            signif(model$bestTune, digits=1), sep=": ", 
                            collapse=" | ")
  return(param_as_str)
}