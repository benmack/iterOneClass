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
fname <- function(base_dir, iter=NULL, method=NULL, ...)
  paste0(base_dir, "/", ifelse(is.null(iter), "", 
                          paste0("i", iter) ), 
  )

sttngs <- list(n_train_pos = 20,
               n_train_un = 100,
               iter_max = "noChange+3",
               indep_un = .5,
               k = 10,
               n_test = 10000,
               seed = 123,
               base_dir = "ignore",
               nPixelsPerTile = 10000,
               expand = 4)

sttngs.bsvm <- list(n_train_pos = 20,
                    n_train_un = 100,
                   iter_max = "noChange+3",
                   indep_un = .5,
                   k = 10,
                   n_test = 10000,
                   seed = 123,
                   base_dir = "ignore",
                   nPixelsPerTile = 10000,
                   expand = 4)

### --------------------------------------------------------
### DATA
### Output: P, PN (optional), U
data(banana)

P <- .banana_trn_pos(banana, sttngs$n_train_pos, sttngs$seed)
banana$x <- raster_scale(x=banana$x, 
                         rng.in=c(0, 1), 
                         rng.out=c(-1,1), 
                         cut.tails=FALSE, 
                         rng.in.from=as.numeric(rownames(P)))
P <- banana$x[][as.numeric(rownames(P)), ]
U <- rasterTiled(banana$x, nPixelsPerTile=sttngs$nPixelsPerTile)
### it is important that the rownames of test_set contain 
### the cell values in un$raster
PN <- .banana_tst_set(banana, sttngs$n_test, seed=sttngs$seed)

### for the demo the image is required as raster on disc
filename_U <- iocc_filename(sttngs$base_dir, 0, "_banana.tif")
writeRaster(banana$x, filename_U, overwrite=TRUE)

### --------------------------------------------------------
### run iocc
cl <- my_registerDoParallel()
iocc <- iterativeOcc(P, U, 
                     iter_max = sttngs$iter_max,
                     n_train_un = sttngs$n_train_un, 
                     k = sttngs$k, 
                     indep_un = sttngs$indep_un,  
                     expand=sttngs$expand,
                     base_dir=sttngs$base_dir,
                     test_set=PN, 
                     seed=123)

### --------------------------------------------------------
### get results of iteration iter
cl <- my_registerDoParallel()
iter=5
iocc.i <- iocc_load(sttngs$base_dir, iter, 
                    filename_U=filename_U)

modelPosition(iocc.i$model)

### --------------------------------------------------------
### get histogram plots of all models for some 
outdir <- paste0(iocc_filename(base_dir=sttngs$base_dir, 
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



### get histograms for all models of a given iteration

U_iter_df <- extract_bandwise(fname_image, U_cells )
source("01b_ioc_allHistograms.R")




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










### benchmark classification
bsvm.nUn <- 
  round(corresponding_samplesize_in_U(iocc.i$n_un, 
                                sum(iocc.i$pred_neg==0),
                                sttngs$n_train_un), 0)

bsvm.model <- trainOcc()
