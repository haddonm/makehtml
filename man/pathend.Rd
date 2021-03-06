% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makehtml_funs.R
\name{pathend}
\alias{pathend}
\title{pathend determines what character is at the end of a path}
\usage{
pathend(inpath)
}
\arguments{
\item{inpath}{the path to be analysed}
}
\value{
the end character of the path; either NA, '/', or "\\"
}
\description{
pathend determines what character is at the end of a
    path uses pathtype to get the seperator and then checks the end
    character
}
\examples{
  indir <- "C:/Users/Malcolm/Dropbox/rcode2/aMSE/data-raw"
  pathend(indir)
}
