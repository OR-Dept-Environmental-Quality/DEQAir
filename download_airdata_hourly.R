library(dplyr)
library(lubridate)
library(feather)   ## feather is a format for fast read & write of large files



#####################################################################################################
#
#               Download & unzip from EPA Airdata - hourly pregenerated files
#                 (the functions below only need to be run once)
#
# input: 
#     yrs_to_get - an array of year for which to download data
#     pollutants_to_get - an array of codes for pollutants to retrieve
#     
# outputs: 
#     requires a data directory (this could be created if it doesn't exist..
#     unzipped files csv file - one for each pollutant for each year
#     feather (a fast file read format) for OR sites
#
# How to use:
# define array of years for which you want the data:
#      yrs_to_get <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
#  the pollutant codes for which you want the data:
#     pollutants_to_get <- c("88502", "44201", "42602" )
# source this file, and callthis function:
#     download_airdata_hourly(yrs_to_get = yrs_to_get,
#                              pollutants_to_get = pollutants_to_get)
# Make sure a folder named "data" exists
#
#####################################################################################################

base_url <- "https://aqs.epa.gov/aqsweb/airdata/hourly_"



## 88502 : PM2.5 (neph)
## 44201 : ozone
## 42602 : nitrogen dioxide

download_airdata_hourly <- function(yrs_to_get, pollutants_to_get) {

  for (pollutant in pollutants_to_get) {
    for (yr in yrs_to_get) {
      file_tail <- paste0(pollutant, "_", yr, ".zip")
      url <- paste0(base_url, file_tail)
      destfile <- paste0("data/hourly_", file_tail)
      download.file(url, destfile)
      unzip(destfile)
    }
  }

  for (pollutant in pollutants_to_get) 
    for (yr in yrs_to_get) {
      file_tail <- paste0(pollutant_to_get, "_", yr)
      file_csv <- ".csv"
      file_feather <- ".feather"
      in_file <- paste0("data/hourly_", file_tail, file_csv)
      out_file <- paste0("data/or_hourly_", file_tail, file_csv)
      out_feather <- paste0("data/or_hourly_", file_tail, file_feather)
      df1 <- read.csv(in_file, stringsAsFactors = FALSE)
      df2 <- filter(df1, State.Code == 41)
      write.csv(df2, out_file, row.names = FALSE )
      write_feather(df2, out_feather )
  }
}
