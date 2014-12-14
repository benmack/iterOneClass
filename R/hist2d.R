#' @export
hist2d <- function(pred_occ, pred_iocc, fname=NULL) {
  df = data.frame(iterOCC=pred_iocc,
                  OCC=pred_occ)
  require(hexbin)
  require(RColorBrewer)
  rf <- colorRampPalette(rev(brewer.pal(11,'RdBu')))
  r <- rf(32)
  # Scaling of legend - must provide both trans and inv functions
  df = data.frame(OCC=pred_occ, iterOCC=pred_iocc)
  h <- hexbinplot(OCC~iterOCC, data=df, colramp=rf,
                  trans=log, inv=exp)
  if (!is.null(fname))
    pdf(fname)
  print(h)
  if (!is.null(fname))
    dev.off()
}
