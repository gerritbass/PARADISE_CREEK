# GERRIT BASS WINTER 2022/23
# SCRIPT TO FORMAT AND COMBINE PARADISE CREEK DATA FILES EXPORTED FROM WISKI

# FOR CLEANED DATA---------------------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(lubridate)

# make a list of all your files
data_files <- grep(list.files('data'), pattern='.log', invert=TRUE, value=TRUE)
data_files <- data_files[ !grepl("other_data|raw", data_files)]


# clean data and create a file to append the rest of the files too

# set the first file equal to a variable "file1"
file1 <- data_files[1]

#extract station name from file name
station<- sapply(strsplit(file1,"_"), `[`, 1) 

# read in data
paradise_file1 <- read.csv(paste('data/',file1, sep = ""), skip = 15,check.names = FALSE)

# clean data
paradise1_cleaned <- paradise_file1 %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>%
  mutate(station = station) %>% 
  dplyr::rename(WT = 3) %>% # needed to specify the rename command from dplyr to work right
  select(station,datetime,WT)

write_csv(paradise1_cleaned, file = "output/appended_paradise.csv", append = FALSE)

# remove the first file from the list of files before appending the rest of the files
data_files_new <- data_files[-1]

#function to clean and append Paradise Creek data to one .CSV file

Paradise_cleanANDcompile <- function(file){
  
  paradise_file <- read.csv(paste('data/',file, sep = ""), skip = 15, check.names = FALSE)
  
  station<- sapply(strsplit(file,"_"), `[`, 1) #grab station name from file name
  
  paradise_cleaned <- paradise_file %>% 
    mutate(datetime = mdy_hms(paste(Date, Time))) %>%
    mutate(station = station) %>% 
    dplyr::rename(water_tempC = 3) %>% 
    select(station,datetime,water_tempC)
  
  write_csv(paradise_cleaned, file = "output/appended_paradise.csv", append = TRUE)
} 

# clean and append the rest of the data to appended_paradise
lapply(data_files_new, Paradise_cleanANDcompile)

#__________________________________________________________________________________

## Now that we have a file with all the data in it we can change it from long to wide format to make it easier to work with and graph
## EDIT: turns out long is actually better so wider commands have been commented out and name changed

#read in our new file and identify NA values
all_paradise <- read_csv("output/appended_paradise.csv", na = "---")

#remove duplicates and pivot wider
long_paradise <- all_paradise %>%
  distinct()# %>% # get rid of duplicates
  #pivot_wider(names_from = station, values_from = 'water_tempC')

#create a new file to work with
write_csv(long_paradise, file = "output/paradiseWT_full.csv")


#FOR RAW DATA---------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(lubridate)

# make a list of all your files
RAW_data_files <- grep(list.files('data/raw'), pattern='.log', invert=TRUE, value=TRUE)

# clean data and create a file to append the rest of the files too

# set the first file equal to a variable "file1"
RAW_file1 <- RAW_data_files[1]

#extract station name from file name
RAW_station<- sapply(strsplit(RAW_file1,"_"), `[`, 1) 

# read in data
RAW_paradise_file1 <- read.csv(paste('data/',RAW_file1, sep = ""), skip = 15,check.names = FALSE)

# clean data
RAW_paradise1_cleaned <- RAW_paradise_file1 %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>%
  mutate(station = station) %>% 
  dplyr::rename(WT = 3) %>% # needed to specify the rename command from dplyr to work right
  select(station,datetime,WT)

write_csv(RAW_paradise1_cleaned, file = "output/appended_paradise_RAW.csv", append = FALSE)

# remove the first file from the list of files before appending the rest of the files
RAW_data_files_new <- RAW_data_files[-1]

#function to clean and append Paradise Creek data to one .CSV file

RAW_Paradise_cleanANDcompile <- function(file){
  
  RAW_paradise_file <- read.csv(paste('data/raw/',file, sep = ""), skip = 15, check.names = FALSE)
  
  station<- sapply(strsplit(file,"_"), `[`, 1) #grab station name from file name
  
  RAW_paradise_cleaned <- RAW_paradise_file %>% 
    mutate(datetime = mdy_hms(paste(Date, Time))) %>%
    mutate(station = station) %>% 
    dplyr::rename(water_tempC = 3) %>% 
    select(station,datetime,water_tempC)
  
  write_csv(RAW_paradise_cleaned, file = "output/appended_paradise_RAW.csv", append = TRUE)
} 

# clean and append the rest of the data to appended_paradise
lapply(RAW_data_files_new, RAW_Paradise_cleanANDcompile)

#__________________________________________________________________________________

## Now that we have a file with all the data in it we can change it from long to wide format to make it easier to work with and graph
## EDIT: turns out long is actually better so wider commands have been commented out and name changed

#read in our new file and identify NA values
RAW_all_paradise <- read_csv("output/appended_paradise_RAW.csv", na = "---")

#remove duplicates and pivot wider
RAW_long_paradise <- RAW_all_paradise %>%
  distinct()# %>% # get rid of duplicates
#pivot_wider(names_from = station, values_from = 'water_tempC')

#create a new file to work with
write_csv(RAW_long_paradise, file = "output/paradiseWT_full_RAW.csv")








  

  
 

