library(dplyr)
library(lubridate)
library(feather)   ## feather is a format for fast read & write of large files
library(ggplot2)
#library(hashmap)    
#library(zeallot)    ## %<-% operator for multiple assignments

source("get_monitor_data.R")
source("create_summaries.R")
source("make_graphs.R")

## global & convenience parameters set up
stn_info <- read.csv("data/stn_info.csv", stringsAsFactors = FALSE)
stn_info$stn_id <- as.numeric(stn_info$stn_id)
stn_info$latitude <- as.numeric(stn_info$latitude)
neph_stns <- read.csv("data/stns_neph.csv", stringsAsFactors = FALSE)
no2_stns <- c(410510080, 410670005)
o3_stns <- c()
#stations <- neph_stns[,2]


## specify the years you want to use in the graphs
yrs_to_get <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

## speicfy the pollutants you want the graphs for
pollutants_to_get <- c("88502")



## The expectation is that the current data has been downloaded from AQI and written out to
## a csv file in a format that can then be easily combined with the data from EPA's AirData or AQS Tech websites.
# write.csv(web_file, "data/site13_pm25_20190101_to_20200331.csv", row.names = FALSE)
# web_file <- read.csv("data/or_neph_pm25_hourly_20190101_to_20200412_working.csv", stringsAsFactors = FALSE)


### loop through all the stations
###     combine data from AirData & AQI
###     create daily summaries (and save as csv in summaries folder)
##      create point, line, and tile graphs based on daily summaries and save in graphs folder  
##  the weekly summaries & graphs (boxplots) can be done in this for loop or in a separate loop
##  change the station list above to add/remove stations to the list
##  it is easier to write a loop for each pollutant as there are different sites fr different pollutants.

## the options for months_to_plot are:
##     month number: 4
##     months: c(1,2,3,4)
##     year: "all"
##     first quarter: "q1"

months_to_plot <- "to4"
no2_stns <- c(410510080, 410670005)
o3_stns <- c(410510080,  410670005)
polls <- c("no2", "o3", "pm25")
for (poll in polls) {
  if (poll == "pm25") {
    stations <- neph_stns[,2]
    pollutant_to_get <- "88502"
    webfile <- "data/or_neph_pm25_hourly_20190101_to_20200412_working.csv"
  }
  if (poll == "o3") {
    stations <- o3_stns
    pollutant_to_get <- "44201"
    webfile <- "data/or_o3_hourly_20190101_to_20200412_working.csv"
  }
  if (poll == "no2") {
    stations <- no2_stns
    pollutant_to_get == "42602"
    webfile <- "data/or_no2_hourly_20190101_to_20200412_working.csv"
  }
  for (sta in stations) {
    sta_name <- tolower(stn_info$code[stn_info$stn_id == sta])
    df10 <- get_stn_data(stn = sta, yrs_to_get, pollutant_to_get)
    coln <- paste0(sta_name, "_", poll)
    df19 <- get_website_data(webfile, sta, coln )
    df <- rbind(df10, df19)
    df <- arrange(df, dt_local)
    df_daily <- create_daily_summary(df)
    city_name <- stn_info$city[stn_info$stn_id == sta]
    tstr <- paste0("Daily Average ",  toupper(poll), " at ", toupper(sta_name),": ", city_name)
    gr_pt <- make_point_graph(df_daily, months_to_plot, tstr)
    gr_pt
    gr_ln <- make_line_graph(df_daily, months_to_plot, 2020, tstr)
    gr_ln
    gr_tl <- make_tile_graph(df_daily, months_to_plot, tstr)
    gr_tl
    fname_dly <- paste0("summaries/", sta_name, "_", poll, "_daily_summary_201001-202004.csv")
    write.csv(df_daily, fname_dly, row.names = FALSE)
    line_dly <- paste0("graphs/", sta_name, "_", poll, "_daily_", months_to_plot, "_line_2020.png")
    png(filename=line_dly, type="cairo", width = 900, height = 560)
    plot(gr_ln)
    dev.off()
    gr_wline <- make_wline_graph(df_daily, months_to_plot, 2020, tstr)
    wline_dly <- paste0("graphs/", sta_name, "_", poll, "_daily_", months_to_plot, "_wline_2020.png")
    png(filename=wline_dly, type="cairo", width = 900, height = 560)
    plot(gr_wline)
    dev.off()
    
    pt_dly <- paste0("graphs/", sta_name, "_", poll,  "_daily_", months_to_plot,"_point_2020.png")
    png(filename=pt_dly, type="cairo", width = 900, height = 560)
    plot(gr_pt)
    dev.off()
    tile_dly <- paste0("graphs/", sta_name, "_", poll, "_daily_", months_to_plot, "_tile_2020.png")
    png(filename=tile_dly, type="cairo", width = 900, height = 560)
    plot(gr_tl)
    dev.off()
  }
}


