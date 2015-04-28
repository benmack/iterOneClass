#' @export
get_puTrainset <- function(P, U, nTrainUn, seed_iter) {
  
  train_un <- sample_rasterTiled(U, size=nTrainUn, seed=seed_iter)
  train_pu_x <- rbind(P, train_un)
  train_pu_y <- puFactor(rep(c(1, 0), 
                             c(nrow(P), nrow(train_un))))
  
  index <- createFoldsPu( train_pu_y, k=kFolds, 
                          indepUn=indepUn, seed=seed_iter)
  return(list(x=train_pu_x,
              y=train_pu_y,
              index=index))
}