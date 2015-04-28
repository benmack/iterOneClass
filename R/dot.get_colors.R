#' @export
.get_colors <- function(what="PU") {
  if (what == "PU") {
    colors <- list(pos='#2166ac', un='#e0e0e0')
  } else if (what == "PN") {
    colors <- list(pos='#2166ac', neg='#f4a582')
  }
  return (colors)
}
  