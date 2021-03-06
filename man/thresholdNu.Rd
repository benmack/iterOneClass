% Generated by roxygen2 (4.0.1): do not edit by hand
\name{thresholdNu}
\alias{thresholdNu}
\title{A conservative threshold for classifying highly likely negatives.}
\usage{
thresholdNu(model, pred, method = "lowerWhisker", expand = 2)
}
\arguments{
\item{model}{an object of class \code{\link{trainOcc}}.}

\item{pred}{the predictions of the unlabeled samples.}

\item{method}{a thresholding method. So far only \code{lowerWhisker} is implemented.}

\item{expand}{change the threshold such that \code{n_un_th*expand} pixels remain unlabeled, where \code{n_un_th} is the number of samples (of \code{pred}) remaining unlabeled when applying the threshold determined by \code{method}.}
}
\description{
...
}

