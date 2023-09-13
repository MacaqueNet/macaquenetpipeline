% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_key.R
\name{get_key}
\alias{get_key}
\title{read the key file to all data sets}
\usage{
get_key(read = TRUE, ready_to_deploy_only = FALSE)
}
\arguments{
\item{read}{logical, should the columns for processing the raw files be
included (\code{TRUE} by default)}

\item{ready_to_deploy_only}{logical, only return data sets that are marked
as ready for deployment in key file. Default is \code{FALSE}, i.e.,
use all rows irrespective of their status.}
}
\value{
a data.frame with the data sets (to be) included in the package
}
\description{
read the key file to all data sets
}
\examples{
key <- get_key()
table(key$species)
colnames(key)

# without the reading columns
key <- get_key(read = FALSE)
colnames(key)
}