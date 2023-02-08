# Gerrit Bass Winter 2022/23
# Homebase for analyzing cleaned Paradise Creek data that has been been exported from WISKI 
# and complied by the "Paradise_from_WISKI" script.

library(plyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)


# READ IN DATA ----------------------------------------------------------------- 

#set directory where files are located
setwd("R:/_04_Project_Data/from_WISKI/Paradise_Creek")

# read in Paradise Creek Water Temp (degC) data
paradise_data <- read_csv("paradiseWT_full.csv") %>% 
  mutate(station = as_factor(station))

paradise_data <- drop_na(paradise_data)

# read in flow data
paradise_flow <- read_csv('R:\\_04_Project_Data\\Water_Quality\\Paradise_Creek_Temp_Study\\Data\\USGS_flow_Paradise_at_UofI.csv') %>% 
  select(1:3) %>%   
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"))

# read in precip data
precip_data <- read_csv('R:\\_04_Project_Data\\Water_Quality\\Paradise_Creek_Temp_Study\\Data\\airport_precip_data.csv') %>%    
  mutate(datetime = as.POSIXct(datetime)) %>% 
  mutate(datetime = round_date(datetime, unit = "hour"))

precip_flow <- merge(precip_data, paradise_flow, by = 'datetime') %>% 
  select(1,3,5)

# BASIC STATS ----------------------------------------------------------------
# overall max, min, mean across all sites daily
daily_stats <- paradise_data %>% 
  mutate(date = date(datetime)) %>% 
  group_by(date) %>% 
  dplyr::summarize(max = max(WT),
                   min = min(WT),
                   mean = mean(WT))

daily_stats_graph <- ggplot(data = daily_stats)+
  geom_line(aes(x = date, y = max),color = "red")+
  geom_line(aes(x = date, y = min),color = "green")+
  geom_line(aes(x = date, y = mean), color = "blue")+
  geom_hline(yintercept = 17.5, color = "red")

#-------------------------------------------------------------------------------

ggplot(data = precip_flow,aes(x = datetime))+
  geom_line(aes(y = Q),color = "red")+
  geom_line(aes(y = Precip),color = "green")



#max, min, and mean WT by station
station_max <- paradise_data %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_mean = mean(WT)) 
station_max

# graph of max, min, and mean WT by station
ggplot(data = station_max)+
  geom_point(aes(x = station, y = station_maxi, fill = station),shape = 24, size = 3)+
  geom_point(aes(x = station, y = station_min, fill = station),shape = 25, size = 3)+
  geom_point(aes(x = station, y = station_mean, fill = station),shape = 22, size = 3)
 
# Daily Median Water Temp across all sites 
daily_median <- paradise_data %>% 
  mutate(date = date(datetime)) %>% 
  group_by(date) %>% 
  dplyr::summarise(daily_median_WT = median(WT))

# graph of daily median temps
ggplot(data = daily_median,aes(x = date, y = daily_median_WT )) +
  geom_line()+
  geom_hline(yintercept = 17.5, color = "red")



#BASIC GRAPHS ---------------------------------------------------------------------

#basic time series graph of all stations
ggplot(data = paradise_data,aes(x = datetime, y = WT, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")
  

#basic boxplot
ggplot(data = paradise_data,aes(x = station, y = WT )) +
  geom_boxplot(aes(color = station))
 
#------------------------------------------------------------------
# 7-DADMax

# daily max
daily_max_temp <- paradise_data %>%
  mutate(date = date(datetime)) %>% #create at date column from datetime
  group_by(date, station) %>% 
  dplyr::summarise(daily_max = max(WT))

# daily max graph
ggplot(data = daily_max_temp,aes(x = date, y = daily_max, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")

# rolling 7 day average of daily max
seven_day_moving_max <- daily_max_temp %>%
  group_by(station) %>%
  mutate(seven_DADMax = rollmean(daily_max,7,fill = NA))

# 7DADMax graph
ggplot(data = seven_day_moving_max,aes(x = date, y = seven_DADMax, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")

# calculates the number of days the 17.5 deg threshold is exeeded
exceed_sum <- seven_day_moving_max %>% 
  drop_na() %>% 
  group_by(station) %>% 
  summarise(exeed_days = sum(seven_DADMax > 17.5), days = sum(seven_DADMax > 0), ratio = exeed_days/days )
exceed_sum

# graph of number of eceedance days ratio for each site
ggplot(data = exceed_sum,aes(x = station, y = ratio )) +
  geom_bar(stat= "identity", aes(fill = station))+
  coord_cartesian(ylim = c(0.5, 0.7))

# Monthly Stats (min, max, and range) ------------------------------------------------------------------------------------------

june_stats <- paradise_data %>%
  filter(month(datetime) %in% 6) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'JUNE')

june_stats

july_stats <- paradise_data %>%
  filter(month(datetime) %in% 7) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'JULY')

aug_stats <- paradise_data %>%
  filter(month(datetime) %in% 8) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'AUG')

sept_stats <- paradise_data %>%
  filter(month(datetime) %in% 9) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'SEPT')

oct_stats <- paradise_data %>%
  filter(month(datetime) %in% 10) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'OCT')

monthly_stats <- rbind(june_stats, july_stats, aug_stats, sept_stats, oct_stats)
monthly_stats

site_maxs <- monthly_stats %>% 
  group_by(station) %>% 
  filter(station_maxi == max(station_maxi))

site_maxs

monthly_maxs <- monthly_stats %>% 
  group_by(month) %>% 
  filter(station_maxi == max(station_maxi))
monthly_maxs

#--------------------------------------------------------------------------------


# Calculate the range needed to avoid having your hyetograph and hydrograph overlap 
maxRange <- 150 # set how wide of the first axis (streamflow)
coeff <- 0.005# set the shrink coeffcient of Precipitation


# Plot the data
flow <- ggplot(data = precip_flow, aes(x = datetime)) +
  geom_line(aes(y = Q))

library(cowplot)

plots <- align_plots(flow,daily_stats_graph)

ggdraw(plots[[1]]) + draw_plot(plots[[2]])



  # Use geom_tile to create the inverted hyetograph
  # y = the center point of each bar
  # maxRange - Precipitation/coeff/2
  geom_tile(aes(y = maxRange - Precip/coeff/2, 
                height = Precip/coeff, 
                fill = 'PColor'
                ))+
  # Plot your discharge data
  geom_line(aes(y = Q), 
            alpha = 0.8,
            size = 0.7) +
  # Create a second axis with sec_axis() and format the labels to display the original precipitation units.
  scale_y_continuous(name = "Streamflow (cfs)",
                     limit = c(0, maxRange),
                     expand = c(0, 0),
                     sec.axis = sec_axis(trans = ~(.-maxRange)*coeff,
                                         name = "Precipitation (in/d)"))+
  scale_fill_manual(values = c('PColor' = "#386cb0"),
                    labels = c('PColor' = 'Precipitation'),
                    name = NULL
  )+
  scale_color_manual(values = c('black', '#e41a1c'), 
                     name = NULL)+
  theme_bw()+
  guides(color = guide_legend(nrow = 1)) +
  theme(
    # legend.position = c(0.75, 0.5),
    legend.position = 'top',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

  
  
  
  
  
  
  
  
  
  
  p1 <- ggplot(mpg, aes(manufacturer, hwy)) + stat_summary(fun.y="median", geom = "bar") +
    theme_half_open() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust= 1))
  p2 <- ggplot(mpg, aes(manufacturer, displ)) + geom_point(color="red") +
    scale_y_continuous(position = "right") +
    theme_half_open() + theme(axis.text.x = element_blank())
  
  # manually align and plot on top of each other
  aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
  
  # Note: In most cases two y-axes should not be used, but this example
  # illustrates how one could accomplish it.
  ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  # }

  
