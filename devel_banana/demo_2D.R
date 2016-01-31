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
### GENERATE DATA
require(MASS)
generateData_2D <- function(parP, parN, n, seed=123) {
  get_sigma <- function(v)
    Sigma <- matrix(c(v, 0, 0, v), 2, 2) 
  y <- c(rep(c(1, -1), c(parP$p*n, sum(parN$p)*n)))
  set.seed(seed)
  x <- mvrnorm(parP$p*n, rep(parP$mu, 2), get_sigma(parP$var))
  if (length(parN$var)==1)
    parN$var <- rep(parN$var, length(parN$mu))
  if (length(parN$mu)!=length(parN$var) |
        length(parN$var)!=length(parN$p))
    stop(sprintf("parN$... |mu|, |var|, |p|: %d, %d, %d", 
                 length(parN$mu), length(parN$var), 
                 length(parN$p)))
  for (i in 1:length(parN$mu)) {
    if (length(parN$mu[i])==1)
      mu <- rep(parN$mu[i], 2)
    set.seed(seed+i)
    ans <- mvrnorm(parN$p[i]*n, mu, get_sigma(parN$var[i]))
    x <- rbind(x, ans)
  }
  return(data.frame(y=y, x1=x[,1], x2=x[,2]))
}

parP <- list(mu=0, var=1, p=0.01)
mu=c(-seq(10, 2, length.out=7)^2, c(8, 14))
var=c(c(7:1), c(1,1))
p <- c(0.6, 0.15, 0.09, 0.05, 0.03, 0.02, 0.01, 0.01, 0.03); sum(p)+0.01; length(p); length(mu)
parN <- list(mu=mu, var=var, p=p)

n=100000
d <- generateData_2D(parP, parN, n)

colU <- rgb(0,0,0, alpha=0.1)
plot(d$x1, d$x2, pch=16, 
     col=colU)

library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(9,'Greys')))
r <- rf(32)[32:1]
k <- kde2d(d$x1, d$x2)
image(k)
k <- kde2d(d$x1, d$x2, n=200)
image(k, col=r)


seed=seed+1; seed # 22, 31

set.seed(seed)
trU.idx <- sample(n, 50)
trU <- d$x[trU.idx]

set.seed(seed)

pdfN <- density(d$x[d$y==-1], n=1000, from=min(d$x), to=max(d$x))
pdfP <- density(d$x[d$y==1], n=1000, from=min(d$x), to=max(d$x))
pdfU <- density(d$x, n=1000, from=min(d$x), to=max(d$x))

xlimInset <- c(c(-5,10))
par(mfrow=c(2,1), mar=c(2, 1, 1, 1))
plot(pdfP$x, pdfN$y*sum(parN$p), type="l", lwd=2, col="red")
lines(pdfP$x, pdfP$y*parP$p, lwd=2, col="blue")
lines(pdfN$x, pdfP$y*parP$p + pdfN$y*sum(parN$p), type="l", lty=4, lwd=1)

rug(trU, ticksize = -0.03)

abline(v=xlimInset)
plot(pdfP$x, pdfN$y*sum(parN$p), type="l", lwd=2, col="red", xlim=xlimInset, ylim=c(0,max(pdfP$y*parP$p*2)))
lines(pdfP$x, pdfP$y*parP$p, lwd=2, col="blue")
lines(pdfN$x, pdfP$y*parP$p + pdfN$y*sum(parN$p), type="l", lty=4, lwd=1)

rug(trU, ticksize = -0.03)







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

