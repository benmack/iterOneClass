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
kFolds =10
sttngs <- list(nTrainPos = 20,
               nTrainUn = 100,
               iterMax = "noChange+3",
               indepUn = .5,
               kFolds = kFolds,
               nTest = 10000,
               seed = 123,
               baseDir = "ignore/demo_minPppAtLw",
               nPixelsPerTile = 10000,
               expand = 3, 
               minPppAtLw=TRUE)
dir.create(sttngs$baseDir)

sttngs.bsvm <- list(nTrainPos = 20,
                    nTrainUn = 100,
                   indepUn = .5,
                   kFolds = kFolds,
                   nTest = 10000,
                   seed = 123,
                   baseDir = "ignore",
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
if (FALSE)
  iocc <- iterativeOcc(P, U, 
                       iterMax = sttngs$iterMax,
                       nTrainUn = sttngs$nTrainUn, 
                       kFolds = sttngs$kFolds, 
                       indepUn = sttngs$indepUn,  
                       expand=sttngs$expand,
                       baseDir=sttngs$baseDir,
                       test_set=PN, 
                       seed=123, 
                       minPppAtLw=sttngs$minPppAtLw)

### --------------------------------------------------------
### --------------------------------------------------------
### get a benchmark result
cl <- my_registerDoParallel(cl)
iter=3


# INPUT
sttngs
iter 
filename_U
train_pos <- P
allowParallel=TRUE
folder_out <- paste0(sttngs$baseDir, "/bench_bsvm/")
test_set <- PN
