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

# require(devtools)
# load_all()
# ls()

### --------------------------------------------------------
### SETTINGS
### Make you settings controling the workflow here...

fname <- function(exp=, iter=NULL, method=NULL, ...)
  paste0(exp, "/", ifelse(is.null(iter), "", 
                          paste0("i", iter) ), 
  )

sttngs <- list(n_train_pos = 20,
               n_train_un = 100,
               iter_max = "noChange+3",
               indep_un = .5,
               k = 10,
               n_test = 10000,
               seed = 123,
               exp = "ignore",
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

### --------------------------------------------------------
### run iocc
cl <- my_registerDoParallel()
iocc <- iterativeOcc(P, U, 
                     iter_max = sttngs$iter_max,
                     n_train_un = sttngs$n_train_un, 
                     k = sttngs$k, indep_un = sttngs$indep_un,  
                     expand=sttngs$expand,
                     base_dir="ignore",
                     test_set=PN, 
                     seed=123)

### --------------------------------------------------------
### for the demo the image is required as raster on disc
filename_U <- iocc_filename("ignore", 0, "_banana.tif")
writeRaster(U$raster, filename_U)

### --------------------------------------------------------
### go to iteration iter
iocc.i <- iocc_load(base_dir, iter, 
                   filename_U=filename_U)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### input

system.file("data/banana.rda", package="oneClass")



############################################################
require(logspline)

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

