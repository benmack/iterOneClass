#'@export
.nReqIfSamplingFromAll <- function( nAll, nSubset, nSamplesFromSubset ) {
  probabilityOfSubset <- nSubset/nAll
  nSamplesFromSubset/probabilityOfSubset
}