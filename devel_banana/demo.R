require(oneClass)
require(raster)
require(rasterTiled)
require(devtools)
require(doParallel)
require(dismo)
require(miscRfunctions)

require(devtools)
load_all()

# install_git("benmack/iterativeOcc")

# register parallel backend
stopCluster(cl)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

data(banana)
n_train_pos <- 30
n_train_un <- 200
indep_un <- .5
k = 10
n_test <- 10000
seed <- 123
folder_out <- "ignore"
nPixelsPerTile <- 100000

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### input

train_pos <- .banana_trn_pos(banana, n_train_pos, seed)
banana$x <- scale_data(x=banana$x, 
                       rng.in=c(0, 1), 
                       rng.out=c(-1,1), 
                       cut.tails=FALSE, 
                       rng.in.from=as.numeric(rownames(train_pos)))
train_pos <- .banana_trn_pos(banana, n_train_pos, seed)

un <- rasterTiled(banana$x, nPixelsPerTile = nPixelsPerTile)

### it is important that the rownames of test_set contain 
### the cell values in un$raster 
test_set <- .banana_tst_set(banana, n_test, seed=seed)

un <- iterativeOcc(train_pos, un, 
                   iter_max = 10,
                   n_train_un = nrow(train_pos)*2, 
                   k = 10, indep_un = 0.5,  
                   expand=4,
                   folder_out="ignore",
                   test_set=test_set, 
                   seed=123)


# 
# 
# 
# 
# ### OLD !!!  
# trainIterOcc <- function (train_pos, un, 
#                           n_train_un, seed, k, indep_un, 
#                           folder_out,
#                           nPixelsPerTile, 
#                           test_set) {
#   
#   if (!is.null(test_set)) {
#     #   index_test <- match(as.numeric(rownames(test_set)), 
#     #                                  un$validCells)
#     pred_neg_test <- vector(mode="integer", length=nrow(test_set))
#   }
#   
#   n_un <- length(un$validCells)
#   
#   fname <- 
#     function (folder_out, iter, extension=".pdf", ...) {
#       paste(folder_out, "/", ..., "_iter",
#             formatC(iter, width=2, flag="0"), extension, sep="")
#     }
#   dir.create(folder_out)
#   
#   pred_neg <- vector(mode="integer", length=n_un)
#   
#   validCells_iter1 <- un$validCells
#   
#   ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#   ### iterate 
#   iter = 0
#   STOP = FALSE
#   
#   while(!STOP) {
#     
#     iter <- iter + 1
#     ### classification
#     train_un <- sample_rasterTiled(un, size=n_train_un, seed=seed)
#     train_pu_x <- rbind(train_pos, train_un)
#     train_pu_y <- puFactor(rep(c(1, 0), 
#                                c(nrow(train_pos), nrow(train_un))))
#     
#     index <- createFoldsPu( train_pu_y, k=k, 
#                             indepUn=indep_un, seed=seed )
#     
#     model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index)
#     
#     pred <- predict(un, model, returnRaster = FALSE)
#     
#     th <- thresholdNu(model, pred)
#     
#     pred_in_pred_neg <- which(pred_neg==0)
#     new_neg_in_pred_neg <- pred_in_pred_neg[pred<th]
#     pred_neg[ new_neg_in_pred_neg ] <- iter
#     
#     if (!is.null(test_set)) {
#       pred_in_pred_neg_test <- which(pred_neg_test==0)
#       ans <- predict(model,
#                      test_set[pred_in_pred_neg_test, -1])
#       pred_test <- rep(1+min(ans), nrow(test_set))
#       pred_test[pred_in_pred_neg_test] <- ans
#       ev <- evaluate(p = pred_test[test_set$y==1], 
#                      a = pred_test[test_set$y==-1])
#     }
#     
#     ### plot diagnostics
#     
#     ### grid
#     ans <- plot(model, plotType="level")
#     pdf(fname(folder_out, iter, ".pdf", "grid"))
#     trellis.par.set(caretTheme())
#     print(ans)
#     dev.off()
#     
#     ### histogram
#     pdf(fname(folder_out, iter, ".pdf", "histogram"))
#     hist(model, pred)
#     abline(v=c(th, attr(th, "th_non_expanded")))
#     dev.off()
#     
#     ### featurespace
#     if (ncol(train_pu_x)==2) {
#       pdf(fname(folder_out, iter, ".pdf", "featurespace"))
#       featurespace(model, thresholds=c(th, attr(th, "th_non_expanded")))
#       dev.off()
#     }
#     
#     ### test
#     if (ncol(train_pu_x)==2) {
#       pdf(fname(folder_out, iter, ".pdf", "eval_pn"))
#       plot(ev, main=paste("max. Kappa:", round(max(ev@kappa), 2)))
#       dev.off()
#     }
#     
#     ### change
#     
#     
#     ### save results of this iteration
#     save(iter, model, pred, th, pred_neg, pred_neg_test, ev, 
#          file=fname(folder_out, iter, ".RData", "results") )
#     
#     if (iter==10)
#       STOP=TRUE
#     
#     mask <- emptyRaster(un$raster)
#     
#     # update un
#     un <- rasterTiled(un$raster, 
#                       mask=un$validCells[pred>=th], 
#                       nPixelsPerTile = nPixelsPerTile)
#     
#   }
#   
# }
# 
# 
# 
# 
