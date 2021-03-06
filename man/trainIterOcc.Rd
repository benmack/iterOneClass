% Generated by roxygen2 (4.0.1): do not edit by hand
\name{trainIterOcc}
\alias{trainIterOcc}
\title{Train a iterative one-class classifier.}
\usage{
trainIterOcc(P, U, nUnTrain = 100, M = NULL, seed = NULL,
  resamplingMethod = "createFoldsPu", k = 10, indepUn = 0.5,
  lastModel = NULL, lastThreshold = NULL, ...)
}
\arguments{
\item{P}{a data frame with the positive training samples.}

\item{U}{a raster of rasterTiled object with the full image data to be classified.}

\item{M}{a mask for the unlabeled samples in U. only required of U is araster object.}

\item{seed}{a seed point}

\item{resamplingMethod}{a character, currently only \code{createFoldsPu} supported}

\item{indepUn}{only used if resamplingMethod is \code{createFoldsPu}. The fraction of the unlabeled training data to be used for validation (\code{0 < indepUn < 1}).}

\item{lastModel}{...}

\item{lastThreshold}{...}
}
\description{
...
}

