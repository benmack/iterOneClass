#' @export
corresponding_samplesize_in_U <- 
  function( nAll, nSubset, nSamplesFromSubset ) {
  nSamplesFromSubset/(nSubset/nAll)
}