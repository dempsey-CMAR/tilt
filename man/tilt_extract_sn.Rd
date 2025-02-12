% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tilt_extract_sn.R
\name{tilt_extract_sn}
\alias{tilt_extract_sn}
\title{Extract sensor serial number from the file name(s)}
\usage{
tilt_extract_sn(files, return_numeric = TRUE)
}
\arguments{
\item{files}{Vector of the tilt meter current and/or temperature file names.
Files must start with the 7-digit sensor serial number. Function will give
a warning if the files have different serial numbers.}

\item{return_numeric}{Logical argument indicating whether to convert the
serial number from a character to numeric.}
}
\value{
Returns the first 7 characters of the file name, which should be the
  sensor serial number.
}
\description{
Extract sensor serial number from the file name(s)
}
