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
kFolds =20
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
allowParallel=FALSE
folder_out <- paste0(sttngs$baseDir, "/bench_bsvm/")
test_set <- PN

one_step_occ <- function(sttngs, iter, filename_U, 
                         allowParallel, folder_out
                         ) {

  time_stopper_model <- time_stopper_predict <- c()

  fname_model <- paste0(folder_out, "iter-", iter, "_model.RData")
  dir.create(folder_out)
  
  
  iocc.i <- iocc_load(sttngs$baseDir, iter,
                         filename_U=filename_U)
  un <- rasterTiled(brick(iocc.i$filename_U), mask=iocc.i$validCells)
  idx.feats <- colnames(iocc.i$model$trainingData)!=".outcome"
  names(un$raster) <- colnames(iocc.i$model$trainingData)[idx.feats]
  
  if (!file.exists(fname_model)) {
  
    idx.obs <- iocc.i$model$trainingData$.outcome=="pos"
    train_pos <- iocc.i$model$trainingData[idx.obs, idx.feats]
    
    n_un_bsvm <- as.integer(corresponding_samplesize_in_U(iocc.i$n_un,
                                               iocc.i$n_un_iter,
                                               sttngs$nTrainPos))
    train_un <- sample_rasterTiled(un, n_un_bsvm, seed=sttngs$seed)
    train_pu_x <- rbind(train_pos, train_un)
    train_pu_y <- puFactor(rep(c(1, 0), c(nrow(train_pos),
                                          nrow(train_un))))
    index <- createFoldsPu(train_pu_y, k=sttngs$kFolds,
                           indepUn=sttngs$indepUn, seed=sttngs$seed)
      
    cat("\tTraining ... ")
    ans <- proc.time()
    ### due to memory issues:
    model <- trainOcc(x=train_pu_x, y=train_pu_y, index=index,
                      tuneGrid=sttngs$tuneGrid, allowParallel = allowParallel)
    time_stopper_model <- rbind(time_stopper_model, (proc.time()-ans)[1:3])
    save(train_pu_x, train_pu_y, n_un_bsvm, index, model, 
         time_stopper_model, file=fname_model)
  } else {
    load(fname_model)
  }
  
  modRow <- modelPosition(model)$row
  fname_pred <- gsub("model", paste0("model-", modRow, "_pred"),
                    fname_model)

  if (!file.exists(fname_pred)) {
  
  cat("\tPrediction ... ")
  ans <- proc.time()
  pred <- predict(un, model, returnRaster = FALSE, 
                  fnameLog = gsub(".RData", "_LOG.txt", 
                                  fname_pred) )
  time_stopper_predict <- rbind(time_stopper_predict, (proc.time()-ans)[1:3])
  save(pred, time_stopper_predict, file=fname_pred)
  
  } else {
    load(fname_pred)
  }
  

  
  
  
  ### GO ON HERE!
  
  if (!is.null(test_set)) {
  cat("\tEvaluation ... ")
  pred_test <- predict(model, test_set[, -1])
  ev <- evaluate(p = pred_test[test_set$y==1], 
                 a = pred_test[test_set$y==-1])
  }

  modelPosition(model)
  model <- update(model, modRow=modelPosition(model, 
                                              modRank=1, 
                                              by="puAuc")$row)
  featurespace(model, th=0)
  hist(model, pred, ylim=c(0,0.01))
  plot(ev)
  }




plot_dignostics.R(model, pred, th, iter, folder_out_benchmark, 
                  test_set=test_set, pred_test=pred_test, 
                  ev=ev)








### --------------------------------------------------------
### --------------------------------------------------------
### get results of iteration iter
cl <- my_registerDoParallel(cl)
iter=3
iocc.i <- iocc_load(sttngs$baseDir, iter, 
                    filename_U=filename_U)

modRow = modelPosition(iocc.i$model, modRank=1, by="pnpAtLw")$row
iocc.i$model <- update(iocc.i$model, modRow=modRow)
hist(iocc.i$model, iocc.i$pred)













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

