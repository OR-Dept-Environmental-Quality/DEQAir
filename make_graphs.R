library(dplyr)
library(lubridate)
library(feather)   ## feather is a format for fast read & write of large files
library(ggplot2)
#library(hashmap)    
library(zeallot)    ## %<-% operator for multiple assignments

## global & convenience parameters set up
stn_info <- read.csv("data/stn_info.csv", stringsAsFactors = FALSE)
stn_info$stn_id <- as.numeric(stn_info$stn_id)
stn_info$latitude <- as.numeric(stn_info$latitude)

year_colors <- c( "2010" = '#d0d0d0',
                  "2011" = '#d0d0d0',
                  "2012" = '#d0d0d0',
                  "2013" = '#d0d0d0',
                  "2014" = '#d0d0d0',
                  "2015" = '#a0a0a0', 
                  "2016" = '#a0a0a0', 
                  "2017" = '#a0a0a0', 
                  "2018" = '#a0a0a0', 
                  "2019" = '#a0a0a0', 
                  "2020" = 'red')



#################################################################################################
#
#  Helper functions
#
##################################################################################################

get_months_to_plot <- function(df, months_to_plot = "all") {
  
  if (months_to_plot == "all" ) {
    return(df)
  }
  if (months_to_plot == "q1" ) {
    q1_df <- df %>% filter(month == 1 | month == 2 | month == 3)
    return(q1_df)
  }
  
  if (months_to_plot == "to4" ) {
    q1_df <- df %>% filter(month == 1 | month == 2 | month == 3 | month == 4)
    return(q1_df)
  }
  
  months_df <- df[FALSE,]
  for (mon in months_to_plot) {
    mon_df <- df %>% filter(month == mon)
    months_df <- rbind(months_df, mon_df)
  }
  return(months_df)
}


## change this to always begin on a Monday...to do that, need to know year...it
get_breaks_labels_for_months <- function(months_to_plot) {
  month_breaks <- c(31,60,91,121,152,182,213,244,274,305,335,366)
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  q1_breaks <- c(1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 78, 85)
  q1_labels <- c("01-Jan", "08-Jan", "15-Jan", "22-Jan", "29-Jan",
                 "05-Feb", "12-Feb", "19-Feb", "26-Feb",
                 "04-Mar", "11-Mar", "18-Mar", "25-Mar")
  to4_breaks <- c(1, 8, 15, 22, 29, 
                  36, 43, 50, 57, 
                  64, 71, 78, 85, 
                  92, 99, 106, 113, 120)
  to4_labels <- c("01-Jan", "08-Jan", "15-Jan", "22-Jan", "29-Jan",
                 "05-Feb", "12-Feb", "19-Feb", "26-Feb",
                 "04-Mar", "11-Mar", "18-Mar", "25-Mar",
                 "01-Apr", "08-Apr", "15-Apr", "22-Apr", "29-Apr")
  
  jan_breaks <- c(1, 8, 15, 22, 29)
  jan_labels <- c("01-Jan", "08-Jan", "15-Jan", "22-Jan", "29-Jan")
  feb_breaks <- c(32, 39, 46, 53, 60)
  feb_labels <- c("01-Feb", "08-Feb", "15-Feb", "22-Feb", "29-Feb")
  mar_breaks <- c(61, 68, 75, 82, 89)
  mar_labels <- c("01-Mar", "08-Mar", "15-Mar", "22-Mar", "29-Mar")
  apr_breaks <- c(92, 99, 106, 113, 120)
  apr_labels <- c("01-Apr", "08-Apr", "15-Apr", "22-Apr", "29-Apr")
  may_breaks <- c(122, 129, 136, 143, 150)
  may_labels <- c("01-May", "08-May", "15-May", "22-May", "29-May")
  jun_breaks <- c(153, 160, 167, 174, 181)
  jun_labels <- c("01-Jun", "08-Jun", "15-Jun", "22-Jun", "29-Jun")
  
  if (months_to_plot == "all") {
    ret_breaks <- month_breaks
    ret_labels <- month_labels
  }
  if (months_to_plot == "q1") {
    ret_breaks <- q1_breaks
    ret_labels <- q1_labels
  }
  if (months_to_plot == "to4") {
    ret_breaks <- to4_breaks
    ret_labels <- to4_labels
  }
  
  
  if (months_to_plot== 1) {
    ret_breaks <- jan_breaks
    ret_labels <- jan_labels
  }
  if (months_to_plot== 2) {
    ret_breaks <- feb_breaks
    ret_labels <- feb_labels
  }
  if (months_to_plot== 3) {
    ret_breaks <- mar_breaks
    ret_labels <- mar_labels
  }
  if (months_to_plot== 4) {
    ret_breaks <- apr_breaks
    ret_labels <- apr_labels
  }
  if (months_to_plot== 2) {
    ret_breaks <- may_breaks
    ret_labels <- may_labels
  }
  return(list(ret_breaks, ret_labels))
}

#################################################################################################
#
#  Graphing functions
#
##################################################################################################


make_point_graph <- function(df, months_to_plot, title_string) {
  plot_df <- get_months_to_plot(df, months_to_plot)
  
  breaks_labels <- get_breaks_labels_for_months(months_to_plot)
  g <- ggplot() + geom_point(data = plot_df, aes(x=dayleap, y = mean, color = as.factor(year )), alpha = 0.8) + 
    scale_color_manual( values = year_colors) +
    ggtitle(title_string) + ylab("PM2.5 (ug/m3") +
    scale_x_continuous( breaks = breaks_labels[[1]],
                        labels = breaks_labels[[2]]) +
    labs(color = "Year") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  return(g)
  
}

make_line_graph <- function(df, months_to_plot, hilit_year, title_string) {
  plot_df <- get_months_to_plot(df, months_to_plot)
  hilit_df <- plot_df %>% filter(year ==  hilit_year)
  stat_df <- plot_df %>% 
    filter(year != hilit_year) %>% 
    group_by(dayleap) %>%
    summarise( min_val = min(mean, na.rm = TRUE),
               max_val = max(mean, na.rm = TRUE),
               pct90 =   quantile(mean, 0.9, na.rm = TRUE),
               pct80 =   quantile(mean, 0.8, na.rm = TRUE),
               pct10 =   quantile(mean, 0.1, na.rm = TRUE),
               pct20 =   quantile(mean, 0.2, na.rm = TRUE))
  breaks_labels <- get_breaks_labels_for_months(months_to_plot)
  g <- ggplot() +
    geom_ribbon(data = stat_df, aes(x=dayleap, 
                                    ymax=pct90, 
                                    ymin=pct10), fill="#aeaeae", alpha=.4) +
    geom_line(data = hilit_df, aes(x = dayleap, y = mean), size = 0.6, color = "red", alpha = 0.6) +
    geom_point(data = hilit_df, aes(x = dayleap, y = mean, shape = wdwe), size = 1.8, color = "red", alpha = 0.8) +
    scale_shape_manual(values = c(16, 1),
                       breaks = c("WD", "WE"),
                       labels = c("week-day", "week-end")) +
    ggtitle(title_string) + ylab("PM2.5 (ug/m3") +
    theme_minimal() + 
    theme(legend.title = element_blank()) +
    scale_x_continuous( breaks = breaks_labels[[1]],
                        labels = breaks_labels[[2]]) +
    theme(axis.title.x = element_blank())
  
}

make_tile_graph <- function(df, months_to_plot, title_string) {
  
  plot_df <- get_months_to_plot(df, months_to_plot)
  breaks_labels <- get_breaks_labels_for_months(months_to_plot)
  plot_df$h <- 50
  color.background <- "transparent" 
  
  g <- ggplot(data = plot_df, aes(dayleap, h )) + 
    geom_tile(aes(fill = mean)) + 
    facet_grid(year ~ . ,
               switch = "y") +
    theme(strip.text.y = element_text(size = 12, angle = 180)) +
    scale_fill_gradient(low = "#ffffb3", high = "#b32d00") +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(legend.background = element_rect(fill = color.background)) +
    theme(axis.title=element_text(size=10,color="#525252", face = "bold"))  +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y =element_blank()) +
    theme(legend.title = element_blank()) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = breaks_labels[[1]], 
                       labels = breaks_labels[[2]]) +
    xlab("") +  ylab("") +
    theme(strip.background = element_rect(fill = color.background)) +
    theme(strip.text = element_text(size = 32, face = "bold", color = "#525252")) +
    theme(panel.spacing.y = unit(0, "pt")) +
    theme(panel.background=element_rect(fill=color.background, color=NA)) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor = element_blank())+
    theme(plot.title=element_text(hjust = 0.5, color = "#525252", size=18, face = "bold")) +
    ggtitle(title_string)
  
}


make_wline_graph <- function(df, months_to_plot, hilit_year, title_string) {
  plot_df <- get_months_to_plot(df, months_to_plot)
  hilit_df <- plot_df %>% filter(year ==  hilit_year)
  stat_df <- plot_df %>% 
    filter(year != hilit_year) %>% 
    group_by(daynum_adj_by_wk) %>%
    summarise( min_val = min(mean, na.rm = TRUE),
               max_val = max(mean, na.rm = TRUE),
               pct90 =   quantile(mean, 0.9, na.rm = TRUE),
               pct80 =   quantile(mean, 0.8, na.rm = TRUE),
               pct10 =   quantile(mean, 0.1, na.rm = TRUE),
               pct20 =   quantile(mean, 0.2, na.rm = TRUE))
  #  breaks_labels <- get_breaks_labels_for_months(months_to_plot)
  g <- ggplot() +
    geom_ribbon(data = stat_df, aes(x=daynum_adj_by_wk, 
                                    ymax=pct90, 
                                    ymin=pct10), fill="#aeaeae", alpha=.4) +
    geom_line(data = hilit_df, aes(x = daynum_adj_by_wk, y = mean), size = 0.6, color = "red", alpha = 0.6) +
    geom_point(data = hilit_df, aes(x = daynum_adj_by_wk, y = mean, shape = wdwe), size = 1.8, color = "red", alpha = 0.8) +
    scale_shape_manual(values = c(16, 1),
                       breaks = c("WD", "WE"),
                       labels = c("week-day", "week-end")) +
    ggtitle(title_string) + ylab("PM2.5 (ug/m3") + xlab("Week of Year") +
    theme_minimal() + 
    theme(legend.title = element_blank()) +
    scale_x_continuous( breaks = c(1, 15, 29, 43, 57, 71, 85, 99, 113, 127),
                        labels = c("01", "03", "05", "07", "09", 
                                   "11", "13", "15", "17", "19" )) 
  #  theme(axis.title.x = element_blank("Week of Year"))
  
}

create_boxplot <- function(df, weeks_to_plot, hilit_year, title_string = "") {
  yrs_df <- df[FALSE, ]
  for (wk in weeks_to_plot) {
    wk_df <- filter(df, weeknum == wk)
    yrs_df <- rbind(yrs_df, wk_df)
  }
  plot_df <- yrs_df %>% filter(year != hilit_year)
  hilit_df <- yrs_df %>% filter(year == hilit_year) 
  prev_df <- yrs_df %>% filter(year == hilit_year - 1)
  prev4w_df <- df %>% filter(year == hilit_year) %>% filter(weeknum == weeks_to_plot[1] - 4)
  g <- ggplot(plot_df) +  
    geom_boxplot(aes(x = "", y = mean)) +
    geom_point(data = hilit_df, aes(x = "", y = mean), col = "red", shape = 8, size = 4) +
    geom_point(data = prev_df, aes(x = "", y = mean), col = "darkgreen", shape = 8, size = 3) +
    geom_point(data = prev4w_df, aes(x = "", y = mean), col = "blue", shape = 8, size = 3) +
    theme_minimal() +
    xlab("") + ylab("PM2.5 ug/m3") +
    ggtitle(title_string)
}


