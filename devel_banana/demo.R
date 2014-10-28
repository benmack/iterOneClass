rm(list=ls())
ls()
require(oneClass)
require(kernlab)
require(pROC)
require(raster)
require(rasterTiled)
require(devtools)
require(doParallel)
require(dismo)
require(miscRfunctions)

require(devtools)
load_all()
ls()

# install_git("benmack/iterativeOcc")

# register parallel backend
try(stopCluster(cl))
cl <- makeCluster(detectCores())
registerDoParallel(cl)

data(banana)
n_train_pos <- 20
n_train_un <- 100
indep_un <- .5
k = 10
n_test <- 10000
seed <- 123
folder_out <- "ignore"
nPixelsPerTile <- 100000
expand <- 4
iter_max <- 10

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### input

P <- .banana_trn_pos(banana, n_train_pos, seed)
banana$x <- raster_scale(x=banana$x, 
                       rng.in=c(0, 1), 
                       rng.out=c(-1,1), 
                       cut.tails=FALSE, 
                       rng.in.from=as.numeric(rownames(P)))
P <- banana$x[][as.numeric(rownames(P)), ]

U <- rasterTiled(banana$x, nPixelsPerTile = nPixelsPerTile)

### it is important that the rownames of test_set contain 
### the cell values in un$raster 
PN <- .banana_tst_set(banana, n_test, seed=seed)

try(stopCluster(cl))
cl <- makeCluster(detectCores())
registerDoParallel(cl)
results <- iterativeOcc(P, U, 
                   iter_max = 'noChange',
                   n_train_un = n_train_un, 
                   k = k, indep_un = indep_un,  
                   expand=expand,
                   folder_out="ignore",
                   test_set=PN, 
                   seed=123)

############################################################
### 




############################################################
require(logspline)

iter=7
results_iter <- load_iterativeOcc( folder_out, iter=7)
attach(results_iter)
U <- rasterTiled(x = U$raster, mask = 
                   validCells[pred_neg==0 | pred_neg == iter])

write_hist(model, U, modRows=NULL, thDepPerf=TRUE, 
           folder_out = paste("ignore/hists_iter-", iter, sep="")) 

subU <- sample_rasterTiled(U, 1000, seed=seed)
pred_subU <- predict(model, subU)
dens_subU <- logspline(pred_subU)
hist(model, pred_subU)
plot(dens_subU, add=TRUE)

