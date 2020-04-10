library(dplyr)
library(lubridate)
library(feather)   ## feather is a format for fast read & write of large files




#####################################################################################################
#
#               Functions secitons
#
#
#####################################################################################################

# takes in an EPA stn_id, years of data to get, and a pollutant code
# returns a dataframe that has all the data for the years requested for the specified pollutant at the given site
# Note - it reads in the feather file format; which the downl function writes out only for OR sites
get_stn_data <- function(stn = 410510080, yrs_to_get = year(today() - 1), pollutant_to_get = "88502") {
  out_df <- data.frame(stn_id = as.numeric(),
                       POC = as.integer(), 
                       dt_local = as.character(),
                       Date.Local = as.character(), 
                       Time.Local= as.character(),
                       Sample.Measurement = as.numeric(),
                       Qualifier = as.character(), stringsAsFactors = FALSE)
  
  n <- length(yrs_to_get)
  for (i in 1:n){    
    yr <- yrs_to_get[i]
    in_file <- paste0("data/or_hourly_", pollutant_to_get, "_", yr, ".feather")
    df <- read_feather(in_file)
    df$stn_id <- df$State.Code*10000000 + df$County.Code*10000 + df$Site.Num
    df$dt_local <- paste0(df$Date.Local, " ", df$Time.Local, ":00")
    df_yr <- df %>% filter(stn_id == stn) %>%
      select(stn_id, POC, dt_local, Date.Local, Time.Local, Sample.Measurement, Qualifier)
    out_df <- rbind(df_yr, out_df)
  }
  return(out_df)
}

## this is a on-of function, which shold be replaced by the ENVI API call
## it expectes the name of the file which has the data and the column to retrieve.
## it returns a data frame that has same format as the AirData 
## These two dataframes can be combined using rbind
get_website_data <- function(file_path, stn, pm25_col){
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  out_df <- data.frame(stn_id = stn,
                       POC = 1, 
                       dt_local = df$dt_local,
                       Date.Local = date(df$dt_local), 
                       Time.Local= "",
                       Sample.Measurement = df[, pm25_col],
                       Qualifier = "", 
                       stringsAsFactors = FALSE)
  return(out_df)
}
