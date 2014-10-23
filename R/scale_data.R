#' @export
scale_data <- function(x, rng.in=c(.02, .98), 
                       rng.out=c(0,1), 
                       cut.tails=TRUE, 
                       rng.in.from=NULL) {
  # scale the values of each band of image x to lie within the range
  # rng.out[1] and rng.out[2]. use cut.tails to scale only the values between
  # the cut.tails[1]-th and cut.tails[2]-th percentiles. values out of that
  # range will be set to the closest data extreme.
  # stats_from cells from which to derive the statistics
#   x=banana$x
#   rng.in=c(0, 1) 
#   rng.out=c(0,1) 
#   cut.tails=FALSE
#   rng.in.from=as.numeric(rownames(train_pos))
  
  x.scaled <- x
  for (ii in 1: nlayers(x) ) {
    vals <- values(x[[ii]])
    if (!is.null(rng.in.from)) {
      mima <- quantile(vals[rng.in.from], 
                       probs=c(rng.in[1], rng.in[2]))
    } else {
      mima <- quantile(vals, probs=c(rng.in[1], rng.in[2]))
    }
    df <- data.frame(x=mima, y=rng.out)
    
    if (cut.tails) {
      vals[vals<mima[1]] <- mima[1]
      vals[vals>mima[2]] <- mima[2]
    }
    
    fit <- lm(y~x, data=df)
    #     plot(df)
    #     abline(fit$coefficients[1], fit$coefficients[2])
    pred <- predict(fit, data.frame(x=vals) )
    
    x.scaled <- setValues(x.scaled, pred, layer=ii)
    #    }
  }
  return(x.scaled)
}