
####Define Functions####

#'Function obtains scaled time series from globsim output for a specified reanalysis dataset, variable and station name, see example below.
#'
#'This function extracts climate time series data from scaled GlobSim output nc files.
#'The user must specify which GlobSim reanalysis, variable, and station to obtain the time series for.
#'Finally, the path to the scaled GlobSim output nc files must be provided.
#'
#' @param dataset String. One of ERA5, ERAI, JRA55 or MERRA2 specifying which reanalysis to use.
#' @param var_name String. Which GlobSim climate variable to return, see 'get_var_names'
#' @param station_name String. Which station 'site' to obtain time series data for, see 'station_names'
#' @param scaled_path String. Path to the directory with the `scaled` output from GlobSim
#' @param time_step Float. The time step (in hours) used for interpolation and scaling of GlobSim time series.
#' @return A data frame with one column representing the time stamp of the observation, and the other the scaled value.
#' @export
get_glob_TS<-function(dataset,var_name,station_name,scaled_path,time_step)
{

  time_step<-sprintf("%.1f",time_step)

  if(dataset == 'ERA5'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_era5_',time_step,'h.nc',sep=''))
  }else if(dataset == 'ERAI'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_erai_',time_step,'h.nc',sep=''))
  }else if(dataset == 'JRA56'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_jra55_',time_step,'h.nc',sep=''))
  }else if(dataset == 'MARRA2'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_merra2_',time_step,'h.nc',sep=''))
  }else{cat('No dataset with that name ...')}

  stations<-ncdf4::ncvar_get(reanalysis,"station_name")

  var_temp<-ncdf4::ncvar_get(reanalysis,var_name)[stations==station_name]

  time<-ncdf4::ncvar_get(reanalysis,"time")

  ts<-as.data.frame(cbind(time,var_temp))

  colnames(ts)<-c("DateTime",var_name)

  if(dataset=='ERA5'){
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime),origin="1900-01-01")
  }else if(dataset=='ERAI'){
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime),origin="1900-01-01")
  }else if(dataset=='JRA55'){
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime*60*60),origin="1800-01-01")
  }else if(dataset == 'MERRA2'){
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime),origin="1980-01-01")
  }else{cat('No dataset with that name ...')}

  return(ts)

}

#'Function for returning the variable names within a specified reanalysis dataset.
#'
#'This function extracts the climate variable names present within a scaled GlobSim nc dataset.
#'
#' @param dataset String. One of ERA5, JRA55 or MERRA2 specifying which reanalysis to use.
#' @param scaled_path String. Path to the directory with the `scaled` output from GlobSim.
#' @param time_step Float. The time step (in hours) used for interpolation and scaling of GlobSim time series.
#' @return A vector of GlobSim climate varible names specific to a given reanalysis dataset.
#' @export
get_var_names<-function(dataset,scaled_path,time_step)
{

  time_step<-sprintf("%.1f",time_step)

  if(dataset == 'ERA5'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_era5_',time_step,'h.nc',sep=''))
  }else if(dataset == 'ERAI'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_erai_',time_step,'h.nc',sep=''))
  }else if(dataset == 'JRA56'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_jra55_',time_step,'h.nc',sep=''))
  }else if(dataset == 'MARRA2'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_merra2_',time_step,'h.nc',sep=''))
  }else{cat('No dataset with that name ...')}

  return(names(reanalysis$var))
}

#'Simple function to print station names form within specified dataset, should be same for all datasets
#'
#'This function extracts the station 'site' names present within a scaled GlobSim nc dataset.
#'
#' @param dataset One of ERA5, JRA55 or MERRA2 specifying which reanalysis to use, in this case results should be the same.
#' @param scaled_path String. Path to the directory with the `scaled` output from GlobSim.
#' @param time_step Float. The time step (in hours) used for interpolation and scaling of GlobSim time series.
#' @return A vector of GlobSim station 'site' names specific to a given reanalysis dataset.
#' @export
station_names<-function(dataset,scaled_path,time_step)
{

  time_step<-sprintf("%.1f",time_step)

  if(dataset == 'ERA5'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_era5_',time_step,'h.nc',sep=''))
  }else if(dataset == 'ERAI'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_erai_',time_step,'h.nc',sep=''))
  }else if(dataset == 'JRA56'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_jra55_',time_step,'h.nc',sep=''))
  }else if(dataset == 'MARRA2'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,'scaled_merra2_',time_step,'h.nc',sep=''))
  }else{cat('No dataset with that name ...')}


  stations<-ncdf4::ncvar_get(reanalysis,"station_name")

  return(stations)

}


