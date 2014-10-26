#'@export
.fname <- 
  function (folder_out, iter, extension=".pdf", ...) {
    paste(folder_out, "/", ..., "_iter",
          formatC(iter, width=2, flag="0"), extension, sep="")
  }