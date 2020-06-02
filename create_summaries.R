library(dplyr)
library(lubridate)


## both functions take as input a dataframe containing multiple years of data for a pollutant at a monitoring site
## the dataframe has the columns that are returned by get_stn_data() and get_website_data() in the get_monitor_data.R file
## The idea is that you download all the files that are available from AirData, then combine them with data from the AQI webiste, 
# to create an upto date dataframe for analysis.


## this takes the hourly data in the input dataframe, and creates daily summaries.
## Columns showing weekday, week-end or weekday, month, year are added to allow graphing in different ways
## day number is represented in two ways - daynumber, in which Feb 29 is day 60 in a leap year, but in other years March 1 is.
## dayleap shifts the daynumber by 1 for all days after Feb 28 in a non-leap year. So a label of 1st March should coincide
## with day 1st March for all year
create_daily_summary <- function(df) {
  df$day <- date(df$dt_local)
  daily_summary <- df %>% group_by(day) %>%
    summarise(count = sum(!is.na(Sample.Measurement)),
              max = max(Sample.Measurement, na.rm = TRUE),
              min = min(Sample.Measurement, na.rm = TRUE),
              mean = mean(Sample.Measurement, na.rm = TRUE),
              sd = sd(Sample.Measurement, na.rm = TRUE),
              stn_id = first(stn_id))
  
  daily_summary$year <- year(daily_summary$day)
  daily_summary$month <- month(daily_summary$day)
  daily_summary$week_day <- wday(daily_summary$day, week_start = 1)
  daily_summary <- daily_summary %>% mutate(wdwe = case_when(week_day %in% c(1,2,3,4,5) ~ "WD", 
                                                             week_day %in% c(6,7) ~ "WE", 
                                                             TRUE~""))
  daily_summary$daynum <- yday(daily_summary$day)
  daily_summary$week_num <- trunc((daily_summary$daynum - 2 + wday(paste0(daily_summary$year, "-01-01"), week_start = 1))/7) + 1
  daily_summary$dayleap <- daily_summary$daynum
  daily_summary$daynum_adj_by_wk <- daily_summary$daynum + wday(paste0(daily_summary$year, "-01-01"), week_start = 1) - 1
  yrs <- unique(daily_summary$year)
  for (yr in yrs) {
    if ( yr/4 - trunc(yr/4) > 0) {
      daily_summary$dayleap[((daily_summary$year == yr) & (daily_summary$daynum > 59))] <- 
        daily_summary$daynum[daily_summary$year == yr & daily_summary$daynum > 59] + 1
    }
  }
  return(daily_summary)
  
}


## this takes the hourly data in the input dataframe, and creates weekly summaries.
## A new week starts on Monday, so the first week of the year will usually have < 7 days
## returns a data frame with weekly summaries
create_weekly_summary <- function(df) {
  df$day <- date(df$dt_local)
  df$year <- year(df$day)
  df$daynum <- yday(df$day)
  df$week_day <-  wday(df$day)
  
  df$weeknum <- trunc((df$daynum - 2 + wday(paste0(df$year, "-01-01"), week_start = 1))/7) + 1
  weekly_summary <- df %>% group_by(year, weeknum) %>%
    summarise(count = sum(!is.na(Sample.Measurement)),
              max = max(Sample.Measurement, na.rm = TRUE),
              min = min(Sample.Measurement, na.rm = TRUE),
              mean = mean(Sample.Measurement, na.rm = TRUE),
              sd = sd(Sample.Measurement, na.rm = TRUE),
              stn_id = first(stn_id))
  return(weekly_summary)
  
}


