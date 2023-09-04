# === ANALYZE PHRASE (PART 1): Performing EDA, creating visualizations and gathering key insights ===

# Custom Colors: Bellabeat color and palette was created to align with Bellabeat's color ("#e38872") for aesthetic reasons
bellabeat_color = "#e38872" # custom color taken from Bellabeat logo
bellabeat_palette <- colorRampPalette(c("#df4c29", "#fae9e5"))(5) # custom monochromatic palette ("#D65232", "#DF775E", "#E89D8A", "#F1C2B6", "#FAE8E3")

# Data Categorization: Dataframes do not include clear categories (i.e. demographics) so categories will be created
# Category Structure: (called `activity_level` as column)
#                    Sedentary          ->       Less than 5,000 steps per day
#                    Barely Active      ->       Between 5,000 and 7,499 steps per day
#                    Somewhat Active    ->       Between 7,500 and 9,999 steps per day
#                    Active             ->       Between 10,000 and 12,499 steps per day
#                    Highly Active      ->       More than 12,500 steps per day
# Citation: "How Many Steps a Day Is Considered Active?" - https://www.medicinenet.com/how_many_steps_a_day_is_considered_active/article.htm

# Averaging: Obtaining daily average steps to place participants in category
daily_average_steps <- daily_activity %>% 
  group_by(id) %>% # groups data by participant id
  # averages total steps, round to nearest whole number and stores in new column
  summarize(average_steps = round(mean(total_steps))) 
head(daily_average_steps) # displays average steps per id


# Classifying: Marking users by daily average steps
participant_activity_level <- daily_average_steps %>% 
  mutate(
    # classifies average steps into categories and stores in new column
    activity_level = case_when( average_steps < 5000 ~ "Sedentary", 
                                average_steps >= 5000 & average_steps < 7500 ~ "Barely Active",
                                average_steps >= 7500 & average_steps < 10000 ~ "Somewhat Active",
                                average_steps >= 10000 & average_steps < 12500 ~ "Active",
                                average_steps >= 12500 ~ "Highly Active" 
    )
  )
head(participant_activity_level) # displays average steps and active level per id

# Taking Percentages: Calculating percentage of how many participants fall into each active level
activity_level_percentage <- participant_activity_level %>% 
  group_by(activity_level) %>% # groups participants by active level
  summarize(total_participants = n()) %>%  # calculates total number of participants
  # calculates percentage of participants in active level and stores in new
  mutate(active_percentage = (total_participants / sum(total_participants))) %>%
  mutate(percentage_labels = percent(active_percentage)) # changes calculated percentages into labeled percentages

# displays active level by percentage in descending order
activity_level_percentage %>% 
  arrange(desc(active_percentage)) %>% 
  head() 

# Re-factoring: Setting order of categories to align with above category guideline and visualization

# Stores old factor levels for preservation
# preservation_activity_level_percentage_factor_levels <- activity_level_percentage$activity_level 
# print(preservation_activity_level_percentage_factor_levels) # displays old factor levels

activity_level_percentage$activity_level <- activity_level_percentage$activity_level %>% 
  factor(levels = c("Highly Active",
                    "Active",
                    "Somewhat Active",
                    "Barely Active",
                    "Sedentary"
                    )
  ) # reorders factor levels for visualization
print(activity_level_percentage$activity_level) # displays new factor levels


# VISUALIZATIONS: Various viz for specific statistical inquiries and gathering key insights

# Viz #1 -> PIE CHART: Which categories have the most participants?
# !!! KEY INSIGHT #1: Most participants fall within Sedentary (24.2), Barely Active (27.3) and Somewhat Active (27.3)

# starts the plot of average active level percentages
ggplot(activity_level_percentage, aes(x = "", y = active_percentage, fill = activity_level)) +
  # first part needed for pie chart, bar chat layer and ensuring that pre-calculated percentages are used
  geom_bar(stat = "identity", width = 1) + 
  # second part needed for pie chart, y-values used for slices and rotation starts at 3'clock, 
  #     using 0 instead of pi/2 for aesthetic reasons
  coord_polar(theta = "y", start = 0) + 
  # centers title (subtitle_ and sets font size to 14 (10)
  theme_void() + # uses void theme for aesthetic reasons
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10)) + 
  # applies custom colors to pie slices (Highly Active = Darkest Tint and so forth)
  scale_fill_manual(values = bellabeat_palette) + 
  geom_text(aes(label = percentage_labels), 
            position = position_stack(vjust = 0.5)) + # adds percentage labels on top of pie slices and centers them
  labs(x = NULL,
       y = NULL, # removes x- and y-axes titles
       fill = "Activity Level", # changes legend title
       title = "Participant Distribution",# adds chart title
       subtitle = "% represents proportion participants that fall into each activity level") # adds chart subtitle

# Viz #2 -> HISTOGRAM: Where do participants average steps fall?
# !!! KEY INSIGHT #2: Distribution of average steps appears to be normal, with a slight tail to the right
mean_value <- mean(daily_average_steps$average_steps) # gives mean value for plotting

# EXPLAINATION FOR `bin_width`: I was having a hard time getting the best binwidth for my histogram. After some research, I found that the
#   Freedman-Diaconis rule works best since `daily_average_steps` has a small amount of observations but large average step values.
#   The binwidth was calculated and stored prior to plotting the histogram for organizational purposes
bin_width <- 2 * (IQR(daily_average_steps$average_steps) / nrow(daily_average_steps)^(1/3)) 

ggplot(daily_average_steps, aes(x = average_steps)) +
  # creates histogram and bins
  geom_histogram(binwidth = bin_width, fill = bellabeat_color, color = bellabeat_palette[5]) + 
  geom_vline(aes(xintercept = mean_value),
             linetype = "dashed",
             linewidth = 1) + # plots line at mean value
  theme_light() + # desired viz theme
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + # centers and enlarges chart title
  labs(x = "Average Steps per Participant",
       y = "Number of Participants",
       title="Histogram of Average Daily Steps") + # adds text to x-,y-axis and chart title
  annotate("text", 
           x = mean_value, 
           y = 0, 
           label = paste0("Mean = ", round(mean_value)),
           fontface = "bold",
           vjust = -40,
           hjust = -0.4) # adds, color, and positions Mean text and value to chart near top and slightly to the right

# Viz #3 -> BAR CHARTS: Which week days are participants most active based on average steps and average distance?
# !!! KEY INSIGHT #3: Participants were most active M, T, W, and S based on steps and achieved ideal distance. Sunday was the least active day.

# adds new column to daily activity data that only includes weekday
weekday_steps_distance <- daily_activity %>% 
  mutate(weekday = weekdays(date)) 

# sets levels: Monday < Tuesday < Wednesday < Thursday < Friday < Saturday < Sunday
weekday_steps_distance$weekday <- weekday_steps_distance$weekday %>% 
  ordered(levels = c("Monday",
                     "Tuesday",
                     "Wednesday",
                     "Thursday",
                     "Friday",
                     "Saturday",
                     "Sunday")) 

# calculates average steps, distance and groups by weekday
weekday_steps_distance <- weekday_steps_distance %>% 
  group_by(weekday) %>% 
  summarize(weekday_average_steps = mean(total_steps),
            weekday_average_distance = mean(total_distance)) # calculates average steps, distance and groups by weekday

ggarrange( # arranges both bar charts into one plot
  ggplot(weekday_steps_distance, aes(weekday, weekday_average_steps)) + # charts for average steps
    # conditional used for coloring only bars that meet ideal levels
    geom_col(aes(fill = ifelse(weekday_average_steps > 7500, "Ideal", "Below Ideal"))) + 
    geom_hline(aes(yintercept = 7500),
               linewidth = 1) + #adds a level at recommended steps and distance
    theme_light() + # desired viz theme
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5), # tilts x-axis text
          plot.title = element_text(hjust = 0.5, size = 14), # centers and enlarges chart title
          plot.subtitle = element_text(hjust = 0.5, size = 10), # centers and shrinks chart subtitle
          legend.position = "none") + # removes legend
    scale_fill_manual(values = c("Ideal" = bellabeat_palette[1], "Below Ideal" = bellabeat_palette[5])) + # supplies fill color for conditional
    labs(x = NULL,
         y = NULL,
         title = "Average Steps per Weekday",
         subtitle = "Recommended daily steps is 7500"), # adds labels
  ggplot(weekday_steps_distance, aes(weekday, weekday_average_distance)) +  # charts for average distance
    # conditional used for coloring only bars that meet ideal levels
    geom_col(aes(fill = ifelse(weekday_average_distance > 5, "Ideal", "Below Ideal"))) + 
    geom_hline(aes(yintercept = 5),
               linewidth = 1) +
    theme_light() + # desired viz theme
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none") + 
    scale_fill_manual(values = c("Ideal" = bellabeat_palette[1], "Below Ideal" = bellabeat_palette[5])) +
    labs(x = NULL,
         y = NULL,
         title = "Average Distance per Weekday",
         subtitle = "Recommended daily distance is 5 miles")
)

# Viz #4 - BAR CHART: What hours are participants most active?
# !!! KEY INSIGHT #4: Users are mostly active between the hours of 8 AM and 7 PM, with most walking occurring 12-2 and 5-7

hourly_steps <-  hourly_steps %>% 
  mutate(date = as.Date(date_time),
         time = format(date_time, "%H")) # separates time from date_time column for aggregating and plotting

hours <- hourly_steps %>% 
  group_by(time) %>% 
  summarize(average_hour_steps = mean(step_total)) # aggregates average steps by hour

ggplot(hours, aes(time, average_hour_steps)) + # plots average steps and time
  geom_col(aes(fill = average_hour_steps)) + # fills using average steps
  theme_light() + # desired viz theme
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + # centers and enlargers chart title
  # colors plot with custom palette and green color for contrast
  scale_fill_gradient(low = "#0dfbb2", high = bellabeat_palette[1]) + 
  labs(x = "Hours",
       y = "Average Steps",
       fill = "Average Hourly Steps", # changes legend title
       title = "Average Steps per Hour") # chart labels

# Viz #5 - CORRELATION CHART: Is there any correlation between participants daily steps and calories logged?
# !!! KEY INSIGHT #5: We are seeing a corr coefficient of .56 for daily steps vs calories, indicating a moderately positive relationship

ggplot(daily_activity, aes(total_steps, calories)) + # plots correlation chart
  geom_jitter(stat = "identity") + # ensures points are not overlaying
  sm_statCorr(color = bellabeat_palette[1],
              corr_method = "pearson") + # adds corr coefficient, stat significance and corr line
  theme_light() + # desired viz theme
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + # centers and enlargers chart title
  labs(x = "Total Steps",
       y = "Calories",
       title = "Total Steps vs Calories") # chart labels

# verifies R- and p- values along with other statistical info
cor.test(daily_activity$total_steps, daily_activity$calories, method = "pearson") 
