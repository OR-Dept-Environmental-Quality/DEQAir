library(dplyr)

create_pm25_comp_stats <- function(stations, hilit_week, hilit_year) {
  out_df <- data.frame(stn_id = as.numeric(), 
                       code = as.character(),
                       city= as.character(),
                       lat = as.numeric(),
                       long = as.numeric(),
                       decade_avg = as.numeric(),
                       prevyr_avg = as.numeric(),
                       hilit_avg = as.numeric())
  file_pre <- "summaries/"
  file_post <- "_pm25_weekly_summary_201001-202003.csv"
  for (sta in stations) {
    code <- tolower(stn_info$code[!is.na(stn_info$stn_id) & stn_info$stn_id == sta])
    fname <- paste0(file_pre, code, file_post)
    stn_df <- read.csv(fname, stringsAsFactors = FALSE)
    dec_av <- stn_df %>% 
      filter(year != hilit_year) %>%
      filter(weeknum == hilit_week) %>%
      summarize( avg = mean(mean, na.rm = TRUE))
    prevyr_av <- stn_df %>% 
      filter(year == (hilit_year - 1)) %>%
      filter(weeknum == hilit_week) %>%
      select(mean)
    hilit_av <- stn_df %>% 
      filter(year == hilit_year) %>%
      filter(weeknum == hilit_week) %>%
      select(mean)
    df <- data.frame(stn_id = sta,
                     code = toupper(code),
                     city = stn_info$city[!is.na(stn_info$stn_id) & stn_info$stn_id == sta],
                     lat = stn_info$latitude[!is.na(stn_info$stn_id) & stn_info$stn_id == sta],
                     long = stn_info$longitude[!is.na(stn_info$stn_id) & stn_info$stn_id == sta],
                     decade_avg = dec_av$avg,
                     prevyr_avg = prevyr_av$mean,
                     hilit_avg = hilit_av$mean)
    out_df <- rbind(out_df, df)
    
  }
  return(out_df)
}

