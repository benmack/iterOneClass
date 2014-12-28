### --------------------------------------------------------
### PACKAGES AND FUNCTIONS
require(iterOneClass)
require(rasterTiled)

require(doParallel)
require(kernlab)
require(pROC)

### --------------------------------------------------------
### SETTINGS controling the workflow here...

### iterative occ
kFolds = 10
nTrainPos = 20
nTrainUn = 100
iterMax = "noChange+3"
indepUn = .5
kFolds = kFolds
nTest = 10000
seed = 123
baseDir = "demo/results_minPppAtLw"
nPixelsPerTile = 10000
expand = 3
minPppAtLw=TRUE

### benchmark, one-step biased svm ???

# sttngs.bsvm <- list(nTrainPos = 20,
#                     nTrainUn = 100,
#                    indepUn = .5,
#                    kFolds = kFolds,
#                    nTest = 10000,
#                    seed = 123,
#                    baseDir = "ignore",
#                    nPixelsPerTile = 10000,
#                    expand = 4)

### --------------------------------------------------------
### 
sttngs <- list(nTrainPos = nTrainPos,
               nTrainUn = nTrainUn,
               iterMax = iterMax,
               indepUn = indepUn,
               kFolds = kFolds,
               nTest = nTest,
               seed = seed,
               baseDir = baseDir,
               nPixelsPerTile = nPixelsPerTile,
               expand = expand, 
               minPppAtLw=minPppAtLw)
dir.create(sttngs$baseDir)

fname <- function(baseDir, iter=NULL, method=NULL, ...)
  paste0(baseDir, "/", ifelse(is.null(iter), "", 
                          paste0("i", iter) ), 
  )

### --------------------------------------------------------
### LOAD DATA
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
### register a parallel backand
cl <- makeCluster(detectCores())
registerDoParallel(cl)

### --------------------------------------------------------
### Run iocc
if (FALSE)
  iocc <- iterativeOcc(P, U, 
                       iter_max = sttngs$iterMax,
                       n_train_un = sttngs$nTrainUn, 
                       k = sttngs$kFolds, 
                       indep_un = sttngs$indepUn,  
                       expand=sttngs$expand,
                       base_dir=sttngs$baseDir,
                       test_set=PN, 
                       seed=sttngs$seed, 
                       minPppAtLw=sttngs$minPppAtLw)

### --------------------------------------------------------
### Select a final model, here for the first iteration where 
### no unlabeled samples have been converted to negatives
iocc_last <- load_last_iter(sttngs, filename_U=filename_U)
iter <- iocc_last$iter_last
iocc <- iocc_last$iocc

### Recall that the model selection so far is based on 
### minPppAtLw which is not necessarily an appropriate 
### selection criteria for the final model:
modelPosition(iocc$model)
### change to puF or puAuc
mp.puF <- modelPosition(iocc$model, modRank=1, by='puF')
mp.puAuc <- modelPosition(iocc$model, modRank=1, by='puAuc')

### --------------------------------------------------------
### It can be useful to investigate the histogram plots of 
### several candidate models, e.g. the
candidateModels.iocc <- c(mp.puF$row, mp.puAuc$row)
for (modRow in candidateModels.iocc)
  res_pred_img <- predict.iOcc(sttngs, iter, modRow,
                               filename_U=filename_U)

### --------------------------------------------------------
### --------------------------------------------------------
### Benchmark result a given iteration
iter=4
folder_out <- paste0(sttngs$baseDir, "/iter-", iter, "/benchmark")
dir.create(folder_out)
bench <- one_step_occ(sttngs, iter, filename_U, PN,
                      folder_out=folder_out, allowParallel=TRUE,
                      modRows = 0, 
                      modRows_iocc=candidateModels.iocc) 




load(paste0(folder_out, "/model-37_pred.RData"))
pred_occ <- pred
load(gsub("benchmark", "predU/model-092.RData", folder_out))

iocc.i <- iocc_load(sttngs$baseDir, iter,
                    filename_U=filename_U, U_as_df = FALSE,
                    status="beforeNegClassification",
                    modRow=92, test_set=PN, th=0)
pred_iocc <- get_full_pred_vector(iocc.i)



