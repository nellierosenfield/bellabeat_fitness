# === PROCESS PHASE: Cleaning and processing datasets, daily_activity and hourly_steps ===

# Data Verification: Ensuring sample size of datasets meets 30 or more requirement
n_unique(daily_activity$Id) # returns 33 as total unique IDs
daily_activity %>% 
  select(Id) %>% 
  distinct() %>%
  count()  # verifies there are 33 unique IDs


n_unique(hourly_steps$Id) # returns 33 as total unique IDs
hourly_steps %>% 
  select(Id) %>% 
  distinct() %>% 
  count() # verifies there are 33 unique IDs

# Data Duplication: Locating any duplicates in dataframes
sum(duplicated(daily_activity)) # returns 0 as total duplicates 
# returns all duplicates (if any) from beginning to end of dataframe; 
#   if found, row stored in new subset that's assigned to duplicates object
duplicates <- daily_activity[duplicated(daily_activity) | duplicated(daily_activity,fromLast = TRUE), ]
head(duplicates) # displays duplicated rows (if any)

sum(duplicated(hourly_steps)) # returns 0 as total duplicates
duplicates <- hourly_steps[duplicated(hourly_steps) | duplicated(hourly_steps,fromLast = TRUE), ] # used for verification
head(duplicates) # displays duplicated rows (if any)

# NA Removal: Removing any rows with missing values
preservation_daily_activity <- daily_activity # original record preservation
count(preservation_daily_activity) # returns previous count of rows (940) prior to removal
daily_activity <- preservation_daily_activity %>% 
  distinct() %>% 
  drop_na() # drops any missing values
count(daily_activity) # returns count of 940 - no missing values found
# returns any rows that contain NAs by applying is.na function across all rows in daily_activity: 
#  if found, stores them in new subset if marked as TRUE and assigned to missing_rows object

missing_rows <- daily_activity[apply(is.na(daily_activity), 1, any), ] # used for verification
head(missing_rows) # displays missing rows (if any)

preservation_hourly_steps <- hourly_steps
count(preservation_hourly_steps) # returns previous count of rows (22099) prior to removal
hourly_steps <- preservation_hourly_steps %>% 
  distinct() %>% 
  drop_na()

count(hourly_steps) # returns count of 22099 - no missing values found
missing_rows <- hourly_steps[apply(is.na(hourly_steps), 1, any), ] # used for verification
head(missing_rows) # displays missing rows (if any)


# Column Name Cleaning: Ensuring that naming conventions of columns are standardized
# ensures columns are lowered, underscored, unique and contains standardized characters
daily_activity <- clean_names(daily_activity) 
colnames(daily_activity) # displays cleaned column names

hourly_steps <- clean_names(hourly_steps)
colnames(hourly_steps)

# PLEASE NOTE: If you would like to see the original column names, please comment the below lines. 
#    `preservation_daily_activity` and `preservation_hourly_steps` created in NA Removal step
# colnames(preservation_daily_activity)
# colnames(preservation_hourly_steps)

# PLEASE NOTE: If you would like to see the original column names, please comment the below lines. `daily_activity_old` and `hourly_steps_old` created in NA Removal step
# colnames(preservation_daily_activity)
# colnames(preservation_hourly_steps)

# Date and Time Consistency: Ensuring that all dates and times are standardized to 'YYYY-MM-DD HH:MM:SS'.
daily_activity <- daily_activity %>% 
  rename(date = activity_date) %>% 
  mutate(date = as.Date(mdy(date)))# renames `activity_date` column to date and standardizes dates to 'YYYY-MM-DD'; no times in this dataframe so only date changed
colnames(daily_activity) # displays column names for confirmation of change

# PLEASE NOTE: `hourly_steps` had to be separated and then merged so that date-time values can be formatted as 'YYYY-MM-DD HH:MM:SS Am/PM'
# REASONING: Using as.POSIXCT prior to splitting and mutating the date-time values will produce NAs because the desired format is different from the original format

hourly_steps <- hourly_steps %>% 
  # separates `activity_hour` column into `date` and `time` columns and extra merges AM/PM back to `time`
  separate(activity_hour, into = c("date", "time"), sep = " ", extra = "merge") %>% 
  mutate(date = as.Date(mdy(date))) %>%  # changes `date` values to 'YYYY-MM-DD' format
  unite(date_time, date, time, sep = " ") %>% # merges `date` and `time` columns into `date_time` column
  # standardizes `date-time` values and sets timezone to UTC for accessibility
  mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %I:%M:%S %p", tz = "UTC")) 
head(hourly_steps) # displays new dttm object with desired formatting

# Anomaly Hunting: Examining peculiar aspects of the dataframes

# `daily_activity` anomalies:
#     - Column `logged_activities_distance` doesn't include a lot of data; 
#           will be removed from dataframe and excluded from analysis
#     - Column `sedentary_active_distance` doesn't include a lot of data but can be useful for analysis; 
#           will be kept in dataframe and included in analysis
skim_without_charts(daily_activity) # quick outline and summary of dataframe
daily_activity <- select(daily_activity, -logged_activities_distance) # removes column `logged_activities_distance`
colnames(daily_activity) # confirms `logged_activities_distance` was removed


skim_without_charts(hourly_steps) # quick outline and summary of dataframe

# `hourly_steps` anomalies:
#     - 25th percentile is 0 for `step_total`, could imply that dataframe has skewness; will verify but no action is needed, just being curious :)
skim_without_charts(hourly_steps) # quick outline and summary of dataframe

# CURIOUS SIDE QUEST: Checking skewness with histogram and summary statistics
hs_stat <- hourly_steps %>% 
  summarize(mean(step_total), # 320
            median(step_total), # 40
            min(step_total), # 0
            max(step_total)) # 10554
print(hs_stat) # based on summary stats alone, `step_total` is right-skewed since the mean is greater than the median

ggplot(hourly_steps, aes(x=step_total)) + 
  geom_histogram(binwidth=1500, fill="blue", color="black") # verifies right-skewness using a histogram


















