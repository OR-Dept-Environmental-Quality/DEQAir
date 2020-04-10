library(dplyr)
library(lubridate)
library(feather)   ## feather is a format for fast read & write of large files
library(ggplot2)
#library(hashmap)    
library(zeallot)    ## %<-% operator for multiple assignments

source("get_monitor_data.R")
source("create_summaries.R")
source("make_graphs.R")

## global & convenience parameters set up
stn_info <- read.csv("data/stn_info.csv", stringsAsFactors = FALSE)
stn_info$stn_id <- as.numeric(stn_info$stn_id)
stn_info$latitude <- as.numeric(stn_info$latitude)
stations <- list(410510080, 410670005, 410670004, 410512011, 
                 410090004, 410670111,410390059, 410030013, 410170120,
                 410290203, 410470041, 410330114, 410650007, 410130100)
#stn_name <- hashmap(c(410510080, 410670005, 410670004, 410512011, 
#                      410090004,410670111, 410390059, 410030013, 410170120,
#                      410290203, 410470041, 410330114, 410650007, 410130100), 
#                    c(  "sel",     "tbc",    "hhf", "pch",    
#                        "sis",     "bhp",    "e99", "ccb", "bps",
#                        "afd",     "ssh",    "gpp", "tdc", "pdp"))
#param <- hashmap(c(88502, 62101, 61103, 61104, 64101, 62201), c("neph", "temp", "ws", "wd", "pr", "rh"))


## specify the years you want to use in the graphs
yrs_to_get <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

## speicfy the pollutants you want the graphs for
pollutants_to_get <- c("88502")



## this part will need to be re-written until we get the ENVI API automation done
## basically, it is reading in all the files that have PM2.5 data, commbining into one dataframe, and writing it out as a csv.
## 
file1 <- read.csv("data/ccb_gpp_ssh_tdc_pdp_pm25_201901010_to_20200331.csv", stringsAsFactors = FALSE)
file2 <- read.csv("data/hhf_afd_pch_bhp_sis_tbc_pm25_20190101_to_20200331.csv", stringsAsFactors = FALSE)
file_e99 <- read.csv("data/e99pm25est.csv", stringsAsFactors = FALSE)
file_sel <- read.csv("data/selpm25est1.csv", stringsAsFactors = FALSE)
file_bps <- read.csv("data/bps_pm25_20200331.csv", stringsAsFactors = FALSE)
web_file <- merge(file1, file2, by= "dt_local", all = TRUE)
web_file <- merge(web_file, file_e99, by= "dt_local", all = TRUE)
web_file <- merge(web_file, file_sel, by= "dt_local", all = TRUE)
web_file <- merge(web_file, file_bps, by = "dt_local", all = TRUE)
write.csv(web_file, "data/site13_pm25_20190101_to_20200331.csv", row.names = FALSE)


### loop through all the stations
###     combine data from AirData & AQI
###     create daily summaries (and save as csv in summaries folder)
##      create point, line, and tile graphs based on daily summaries and save in graphs folder  
##  the weekly summaries & graphs (boxplots) can be done in this for loop or in a separate loop
##  change the station list above to add/remove stations to the list
##  this for loop can be embedded in a pollutant loop to generate graphs for all pollutants of interest

for (sta in stations) {
  sta_name <- stn_info$code[stn_info == sta]
  df10 <- get_stn_data(stn = sta, yrs_to_get)
  coln <- paste0(sta_name, "_pm25")
  df19 <- get_website_data("data/site13_pm25_20190101_to_20200331.csv", sta, coln )
  df <- rbind(df10, df19)
  df <- arrange(df, dt_local)
  df_daily <- create_daily_summary(df)
  city_name <- stn_info$city[stn_info$stn_id == sta]
  tstr <- paste0("Daily Average PM2.5 at ", toupper(sta_name),": ", city_name)
  gr_pt <- make_point_graph(df_daily, "q1", tstr)
  gr_pt
  gr_ln <- make_line_graph(df_daily, "q1", 2020, tstr)
  gr_ln
  gr_tl <- make_tile_graph(df_daily, "q1", tstr)
  gr_tl
  fname_dly <- paste0("daily/", sta_name, "_daily_summary_201001-202003.csv")
  write.csv(df_daily, fname_dly, row.names = FALSE)
  line_dly <- paste0("graphs/", sta_name, "_line_q12020.png")
  png(filename=line_dly, type="cairo", width = 900, height = 560)
  plot(gr_ln)
  dev.off()
  pt_dly <- paste0("graphs/", sta_name, "_point_q12020.png")
  png(filename=pt_dly, type="cairo", width = 900, height = 560)
  plot(gr_pt)
  dev.off()
  tile_dly <- paste0("graphs/", sta_name, "_tile_q12020.png")
  png(filename=tile_dly, type="cairo", width = 900, height = 560)
  plot(gr_tl)
  dev.off()
}

