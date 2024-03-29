% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vecapply.R
\name{vecapply}
\alias{vecapply}
\title{Apply a function over two or more vectors with different length}
\usage{
vecapply(x = list(), FUN)
}
\arguments{
\item{x}{list, including two or more vectors}

\item{FUN}{the function to be applied to each element of x}
}
\description{
vecapply returns a list of the same length as the biggest length among all
vectors, each elament of which is the result of applying FUN to each element
of all vectors
}
\author{
Kevin Hu \email{kevinhu@bu.edu}
}
