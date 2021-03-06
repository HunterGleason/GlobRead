% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GlobRead.R
\name{get_glob_TS}
\alias{get_glob_TS}
\title{Function obtains scaled time series from globsim output for a specified reanalysis dataset, variable and station name, see example below.}
\usage{
get_glob_TS(dataset, var_name, station_name, scaled_path, time_step)
}
\arguments{
\item{dataset}{String. One of ERA5, ERAI, JRA55 or MERRA2 specifying which reanalysis to use.}

\item{var_name}{String. Which GlobSim climate variable to return, see 'get_var_names'}

\item{station_name}{String. Which station 'site' to obtain time series data for, see 'station_names'}

\item{scaled_path}{String. Path to the directory with the \code{scaled} output from GlobSim}

\item{time_step}{Float. The time step (in hours) used for interpolation and scaling of GlobSim time series.}
}
\value{
A data frame with one column representing the time stamp of the observation, and the other the scaled value.
}
\description{
This function extracts climate time series data from scaled GlobSim output nc files.
The user must specify which GlobSim reanalysis, variable, and station to obtain the time series for.
Finally, the path to the scaled GlobSim output nc files must be provided.
}
