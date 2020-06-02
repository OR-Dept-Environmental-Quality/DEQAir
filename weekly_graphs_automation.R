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
stations <- neph_stns[,2]

#param <- hashmap(c(88502, 62101, 61103, 61104, 64101, 62201), c("neph", "temp", "ws", "wd", "pr", "rh"))


## specify the years you want to use in the graphs
yrs_to_get <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018 )



week_to_plot <- 14
months_to_plot <- "to4"
no2_stns <- c(410510080, 410670005)
o3_stns <- c(410510080,  410670005)
polls <- c("no2", "o3")
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
    df10 <- get_stn_data(stn = sta, yrs_to_get)
    coln <- paste0(sta_name, "_", poll)
    df19 <- get_website_data(webfile, sta, coln )
    df <- rbind(df10, df19)
    df <- arrange(df, dt_local)
    df_weekly <- create_weekly_summary(df)
    fname_wkly <- paste0("summaries/", sta_name, "_", poll, "_weekly_summary_201001-202003.csv")
    write.csv(df_weekly, fname_wkly, row.names = FALSE)
    
    city_name <- stn_info$city[stn_info$stn_id == sta]
    tstr <- paste0("Average ", toupper(poll), " Week ", as.character(week_to_plot), " ", toupper(sta_name),": ", city_name)
    gr_box <- create_boxplot(df_weekly, week_to_plot, 2020, title_string = tstr)
    gr_box
    box_wkly <- paste0("graphs/", sta_name, "_", poll, "_box_week", as.character(week_to_plot), "_2020.png")
    png(filename=box_wkly, type="cairo", width = 400, height = 450)
    plot(gr_box)
    dev.off()
  }
  
}


