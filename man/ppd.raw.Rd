% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppd.raw.r
\docType{data}
\name{ppd.raw}
\alias{ppd.raw}
\title{Public plans database - raw version.}
\format{Data frame with 1 row per plan per year, with more than 200 variables. See ppdvars for documentation on variables.}
\source{
Center for State and Local Government Excellence (SLGE),\cr
        National Association of State Retirement Administrators (NASRA), and\cr
        Center for Retirement Research at Boston College\cr\cr
        http://publicplansdata.org/public-plans-database/download-full-data-set/\cr
}
\usage{
ppd.raw
}
\description{
Information on 150 large public pension plans, annual, 2001-2015.
}
\examples{
library(dplyr)
glimpse(ppd.raw)
}
\keyword{datasets}
