### --------------------------------------------------------
### PACKAGES AND FUNCTIONS
require(oneClass)
require(kernlab)
require(pROC)
require(raster)
require(rasterTiled)
require(devtools)
require(doParallel)
require(dismo)
require(miscRfunctions)
require(logspline)

require(devtools)
load_all()
ls()

### --------------------------------------------------------
### SETTINGS
### Make you settings controling the workflow here...
fname <- function(baseDir, iter=NULL, method=NULL, ...)
  paste0(baseDir, "/", ifelse(is.null(iter), "", 
                          paste0("i", iter) ), 
  )

sttngs <- list(nTrainPos = 20,
               nTrainUn = 100,
               iterMax = "noChange+3",
               indepUn = .5,
               kFolds = 10,
               nTest = 10000,
               seed = 123,
               baseDir = "ignore/demo",
               nPixelsPerTile = 10000,
               expand = 4)
dir.create(sttngs$baseDir)

sttngs.bsvm <- list(nTrainPos = 20,
                    nTrainUn = 100,
                   iterMax = "noChange+3",
                   indepUn = .5,
                   kFolds = 10,
                   nTest = 10000,
                   seed = 123,
                   baseDir = "ignore/demo",
                   nPixelsPerTile = 10000,
                   expand = 4)

### --------------------------------------------------------
### DATA
### Output: P, PN (optional), U
data(banana)

P <- .banana_trn_pos(banana, sttngs$nTrainPos, sttngs$seed)
banana$x <- raster_scale(x=banana$x, 
                         rng.in=c(0, 1), 
                         rng.out=c(-1,1), 
                         cut.tails=FALSE, 
                         rng.in.from=as.numeric(rownames(P)))
P <- banana$x[][as.numeric(rownames(P)), ]
U <- rasterTiled(banana$x, nPixelsPerTile=sttngs$nPixelsPerTile)
### it is important that the rownames of test_set contain 
### the cell values in un$raster
PN <- .banana_tst_set(banana, sttngs$nTest, seed=sttngs$seed)

### for the demo the image is required as raster on disc
filename_U <- iocc_filename(sttngs$baseDir, 0, "_banana.tif")
writeRaster(banana$x, filename_U, overwrite=TRUE)

### --------------------------------------------------------
### run iocc
cl <- my_registerDoParallel()
iocc <- iterativeOcc(P, U, 
                     iterMax = sttngs$iterMax,
                     nTrainUn = sttngs$nTrainUn, 
                     kFolds = sttngs$kFolds, 
                     indepUn = sttngs$indepUn,  
                     expand=sttngs$expand,
                     baseDir=sttngs$baseDir,
                     test_set=PN, 
                     seed=123)

### --------------------------------------------------------
### --------------------------------------------------------
### get results of iteration iter
cl <- my_registerDoParallel()
iter=5
iocc.i <- iocc_load(sttngs$baseDir, iter, 
                    filename_U=filename_U)

modelPosition(iocc.i$model)

### --------------------------------------------------------
### get histogram plots of all models 
outdir <- paste0(iocc_filename(baseDir=sttngs$baseDir, 
                               iter=iter), "/otherModels/")

modelPosition(iocc.i$model)
rm(U)
cl <- my_registerDoParallel()
foreach(mm = 1:nrow(iocc.i$model$results), 
        .packages=c("oneClass", "kernlab"), 
        .verbose=TRUE) %dopar% write_hist(iocc.i$model, 
                                          iocc.i$U_df, 
                                          modRows=mm, 
                                          thDepPerf=TRUE, 
                                          folder_out=outdir)


### --------------------------------------------------------
### get benchmark classification
bsvm <- list()
bsvm$outdir <- paste0(iocc_filename(baseDir=sttngs$baseDir, 
                                    iter=iter), "/bsvm/")
bsvm$nUn <- 
  round(corresponding_samplesize_in_U(iocc.i$n_un, 
                                      sum(iocc.i$pred_neg==0),
                                      sttngs$nTrainUn), 0)
set.seed(sttngs$seed)

bsvm$U_tr = sample_rasterTiled(U, bsvm$nUn, seed=sttngs$seed)
bsvm$
bsvm.model <- trainOcc(P)










############################################################

iter=5
results_iter <- load_iterativeOcc( folder_out, iter=iter)
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

