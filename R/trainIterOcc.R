#' @name trainIterOcc
#' @title Train a iterative one-class classifier. 
#' @description ...
#' @param P a data frame with the positive training samples.
#' @param U a raster of rasterTiled object with the full image data to be classified.
#' @param M a mask for the unlabeled samples in U. only required of U is araster object.
#' @param seed a seed point  
#' @param resamplingMethod a character, currently only \code{createFoldsPu} supported
#' @param indepUn only used if resamplingMethod is \code{createFoldsPu}. The fraction of the unlabeled training data to be used for validation (\code{0 < indepUn < 1}).
#' @param lastModel ...
#' @param lastThreshold ...
#' @export
trainIterOcc <- 
  function(P, U, nUnTrain=100, M=NULL, seed=NULL,
           resamplingMethod="createFoldsPu", k=10, indepUn=.5, 
           lastModel=NULL, lastThreshold=NULL, ...) {
  ### modified from 
  # D:\github\iterativeOcc\R\trainModel
  
  # a rasterTiled object of the unlabeled data  
  if (class(U)=="RasterStack" | class(U)=="RasterBrick")
    U <- rasterTiled(U, mask=M, nPixelsPerTile=500000)
  
  # PU-training set 
  tr.un.x <- sample_rasterTiled(U, size=nUnTrain, seed=seed)
  
  PU.x <- rbind(P, tr.un.x)
  PU.y <- puFactor(rep(c(1, 0), c(nrow(P), nrow(tr.un.x))))
  
    # resampling indices
  if (resamplingMethod=="createFoldsPu") {
    if (!is.null(seed))
      set.seed(seed)
    index <- createFoldsPu( PU.y, k=k, indepUn=indepUn )
  }
  
  ### --------------------------------------------------------------------------
  
  ### RUN CLASSIFICATION
  dong <- proc.time()
    oc <- trainOcc(x=PU.x, y=PU.y, index=index, ...)
  ding <- proc.time()
  time.train <- ding-dong
  
  if (is.null(lastModel=NULL)) {
    iter <- 1
  } else {
    iter <- length( oc$previousModels )
  }
  
  oc$iter = iter
  if (!is.null(lastModel)) {
    if (!is.null(lastModel$previousModels)) {
      oc$previousModels <- lastModel$previousModels
    }
    lastModel$previousModels <- NULL
    lastModel$pred <- NULL
    lastModel$resample <- NULL
    lastModel$threshold <- lastThreshold
    
    oc$previousModels[[length(oc$previousModels)+1]]  <- lastModel
    attr(oc, which="class") <- c("trainIterOcc", class(oc))
    names(oc$previousModels) <- paste("iter", 1:length(oc$previousModels), sep=".")
  } else {
    oc$previousModels <- NULL
  }
  
  return(oc)
}
