
####Define Functions####

#'Function obtains scaled time series from globsim output for a specified reanalysis dataset, variable and station name, see example below.
#'
#'This function extracts climate time series data from scaled GlobSim output nc files.
#'The user must specify which GlobSim reanalysis, variable, and station to obtain the time series for.
#'Finally, the path to the scaled GlobSim output nc files must be provided.
#'
#' @param dataset One of ERA5, JRA55 or MERRA2 specifying which reanalysis to use.
#' @param var_name Which GlobSim climate variable to return, see 'get_var_names'
#' @param station_name Which station 'site' to obtain time series data for, see 'station_names'
#' @return A data frame with one column representing the time stamp of the observation, and the other the scaled value.
#' @export
get_glob_TS<-function(dataset,var_name,station_name,scaled_path)
{

  if(dataset == 'ERA5'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_era5_1.0h.nc",sep=""))
  }else if(dataset == 'JRA55'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_jra55_1.0h.nc",sep=""))
  }else{
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_merra2_1.0h.nc",sep=""))
  }

  stations<-ncdf4::ncvar_get(reanalysis,"station_name")

  var_temp<-ncdf4::ncvar_get(reanalysis,var_name)[stations==station_name]

  time<-ncdf4::ncvar_get(reanalysis,"time")

  ts<-as.data.frame(cbind(time,var_temp))

  colnames(ts)<-c("DateTime",var_name)

  if(dataset=='ERA5'){
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime),origin="1900-01-01")
  }else if(dataset=='JRA55'){
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime*60*60),origin="1800-01-01")
  }else{
    ts$DateTime<-lubridate::as_datetime(c(ts$DateTime),origin="1980-01-01")
  }

  return(ts)

}

#'Function for returning the variable names within a specified reanalysis dataset.
#'
#'This function extracts the climate variable names present within a scaled GlobSim nc dataset.
#'
#' @param dataset One of ERA5, JRA55 or MERRA2 specifying which reanalysis to use.
#' @return A vector of GlobSim climate varible names specific to a given reanalysis dataset.
#' @export
get_var_names<-function(dataset,scaled_path)
{
  if(dataset == 'ERA5'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_era5_1.0h.nc",sep=""))
  }else if(dataset == 'JRA55'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_jra55_1.0h.nc",sep=""))
  }else{
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_merra2_1.0h.nc",sep=""))
  }

  return(names(reanalysis$var))
}

#'Simple function to print station names form within specified dataset, should be same for all datasets
#'
#'This function extracts the station 'site' names present within a scaled GlobSim nc dataset.
#'
#' @param dataset One of ERA5, JRA55 or MERRA2 specifying which reanalysis to use, in this case results should be the same.
#' @return A vector of GlobSim station 'site' names specific to a given reanalysis dataset.
#' @export
station_names<-function(dataset,scaled_path)
{
  if(dataset == 'ERA5'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_era5_1.0h.nc",sep=""))
  }else if(dataset == 'JRA55'){
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_jra55_1.0h.nc",sep=""))
  }else{
    reanalysis <-ncdf4::nc_open(paste(scaled_path,"scaled_merra2_1.0h.nc",sep=""))
  }


  stations<-ncdf4::ncvar_get(reanalysis,"station_name")

  return(stations)

}

####Example of use, un-comment####

# #Example of 'get_glob_TS' function use, must provide reanalysis short name, desired reanalysis variable and desired station name.
#
# #E.g., List stations, should be same for all datasets
# station_names('ERA5')
# station_names('JRA55')
# station_names('MERRA2')
#
# #E.g., List variable names for ERA5
# get_var_names('ERA5')
#
#
# #Get 2-m air temp for each reanalysis at the 'atl_sch' station location, plot each time series
# era5_temp<-get_glob_TS('ERA5','AIRT_ERA5_C_sur','atl_sch')
# jra55_temp<-get_glob_TS('JRA55','AIRT_JRA55_C_sur','atl_sch')
# merra2_temp<-get_glob_TS('MERRA2','AIRT_MERRA2_C_sur','atl_sch')
#
# plot(era5_temp$DateTime,era5_temp$AIRT_ERA5_C_sur,type='l')
# lines(jra55_temp$DateTime,jra55_temp$AIRT_JRA55_C_sur,col='blue')
# lines(merra2_temp$DateTime,merra2_temp$AIRT_MERRA2_C_sur,col='red')
