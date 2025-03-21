% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tilt_open_trimdates_app.R
\name{tilt_open_trimdates_app}
\alias{tilt_open_trimdates_app}
\title{Open interactive plot}
\usage{
tilt_open_trimdates_app(
  dat,
  vars = NULL,
  filter_to = c("start", "end", "custom"),
  period = "2 days",
  custom_start = NULL,
  custom_end = NULL,
  point_size = 2
)
}
\arguments{
\item{dat}{Data frame of tilt data in wide format. Must include columns
timestamp_, and the variables to plot (e.g., temperature_degree_c).}

\item{vars}{Character vector of variables to plot. Default is
c("sea_water_speed_cm_s", "sea_water_to_direction_degree",
"temperature_degree_c").}

\item{filter_to}{Shortcut for specifying where to filter \code{dat} before
plotting. Options are "start", "end", or "custom".}

\item{period}{Character string that can be converted to a \code{lubridate}
period. Default is \code{"2 days"}.}

\item{custom_start}{Only required if \code{filter_to = "custom"}. POSIXct
object indicating where the filtered data will begin.}

\item{custom_end}{Only required if \code{filter_to = "custom"}. POSIXct
object indicating where the filtered data will end.}

\item{point_size}{Size of points in the plot.}
}
\value{
Opens a shiny app displaying an interactive plot of variables in
  \code{dat}, coloured by depth.
}
\description{
Open interactive plot
}
