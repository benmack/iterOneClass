rm(list=ls())
ls()
require(oneClass)
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
n_train_pos <- 30
n_train_un <- 60
indep_un <- .5
k = 10
n_test <- 10000
seed <- 123
folder_out <- "ignore"
nPixelsPerTile <- 100000

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### input

P <- .banana_trn_pos(banana, n_train_pos, seed)
banana$x <- scale_data(x=banana$x, 
                       rng.in=c(0, 1), 
                       rng.out=c(-1,1), 
                       cut.tails=FALSE, 
                       rng.in.from=as.numeric(rownames(P)))
P <- banana$x[][as.numeric(rownames(P)), ]

U <- rasterTiled(banana$x, nPixelsPerTile = nPixelsPerTile)

### it is important that the rownames of test_set contain 
### the cell values in un$raster 
PN <- .banana_tst_set(banana, n_test, seed=seed)

un <- iterativeOcc(P, U, 
                   iter_max = 10,
                   n_train_un = n_train_un, 
                   k = 10, indep_un = 0.5,  
                   expand=4,
                   folder_out="ignore",
                   test_set=PN, 
                   seed=123)

