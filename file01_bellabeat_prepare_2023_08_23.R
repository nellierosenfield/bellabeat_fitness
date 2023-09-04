# === PREPARE PHRASE: Setting up work environment, installing/loading packages, load datasets, performing examination ===

# SETUP: Ensuring working directory is correct

# Switching working directory to designated Bellabeat working folder for analysis
setwd('C:/Users/Nellie Rosenfield/OneDrive/Documents/Data Analysis Documents/R Related Files/Google Certificate Case Study 1/case01_bellabeat-fitness/BellaBeat_Working_Analysis')

# PACKAGE INSTALLATION AND LOADING: Installing and loading all necessary packages for analysis process

# Adds needed packages are to vector list and assigning to variable to cut down on work
packages <- c("dplyr", 
              "tidyverse", 
              "ggplot2", 
              "skimr", 
              "readr", 
              "lubridate",
              "janitor",
              "ggpubr",
              "ggrepel",
              "scales",
              "remotes")

# Supplies package variable to install.packages function for bulk installation
install.packages(packages)


# Supplies package variable to lapply to load all required packages at once
lapply(packages, require, character.only=TRUE)

# This remote package is needed for correlation viz ( For more info, https://rdrr.io/github/smin95/sesplot/man/sm_statCorr.html)
remotes::install_github("smin95/sesplot")
library(smplot2)

# DATA EXAMINATION: Examining file contains for overview notes and understanding

# Assigns CSV files to dataframe variables for ease
daily_activity <- read_csv("dailyActivity_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

#daily_calories <- read_csv("dailyCalories_merged.csv")
#daily_intensities <- read_csv("dailyIntensities_merged.csv")
#daily_steps <- read_csv("dailySteps_merged.csv")
#heartrate_seconds <- read_csv("heartrateSeconds_merged.csv")
#hourly_calories <- read_csv("hourlyCalories_merged.csv")
#hourly_intensities <- read_csv("hourlyIntensities_merged.csv")
#minute_calories_long <- read_csv("minuteCaloriesNarrow_merged.csv")
#minute_calories_wide <- read_csv("minuteCaloriesWide_merged.csv")
#minute_intensities_long <- read_csv("minuteIntensitiesNarrow_merged.csv")
#minute_intensities_wide <- read_csv("minuteIntensitiesWide_merged.csv")
#minute_sleep <- read_csv("minuteSleep_merged.csv")
#minute_steps_long <- read_csv("minuteStepsNarrow_merged.csv")
#minute_steps_wide <- read_csv("minuteStepsWide_merged.csv")
#daily_sleep <- read_csv("sleepDay_merged.csv")
#weight_log_info <- read_csv("weightLogInfo_merged.csv")

# Examining larger files that could not be opened with Excel deeper

# heartrateSeconds.csv
heartrate_seconds <- read_csv("heartrateSeconds_merged.csv")
head(heartrate_seconds) # provides first 6 rows for quick glance
n_unique(heartrate_seconds$Id) # returns 14 as total unique IDs
heartrate_seconds %>% 
  select(Id) %>% 
  distinct() %>%
  count() # verifies there are 14 unique IDs

n_unique(heartrate_seconds$Time) # returns 961274 as total unique Times
heartrate_seconds %>% 
  select(Time) %>% 
  distinct() %>%
  count() # verifies there are 961274 unique Times


# minutesMETsNarrow.csv
minutes_mets_long <- read_csv("minuteMETsNarrow_merged.csv")
head(minutes_mets_long)
n_unique(minutes_mets_long$Id) # returns 33 as total unique IDs
minutes_mets_long %>% 
  select(Id) %>%
  distinct() %>% 
  count() # verifies there are 33 unique IDs

n_unique(minutes_mets_long$ActivityMinute) # returns 44160 as total unique minutes
minutes_mets_long %>% 
  select(ActivityMinute) %>% 
  distinct() %>%
  count() # verifies there are 44160 unique minutes





