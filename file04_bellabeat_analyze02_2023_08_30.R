# === ANALYZE PHRASE (PART 2): Performing EDA, creating visualizations and developing marketing strategy ===

# DAILY USAGE: Exploring how often participants logged their activity to understand how often they used their device during the 31 days

daily_usage <- daily_activity %>% 
  group_by(id) %>% # groups rows by id
  summarize(num_days_logged = sum(n())) %>% # sums each participant's row
  # determines participants usage level based on summation
  mutate(usage_level = case_when( num_days_logged == 0 ~ "No Usage", 
                                  num_days_logged >= 1 & num_days_logged < 10 ~ "Minimal Usage",
                                  num_days_logged >= 10 & num_days_logged < 20 ~ "Moderate Usage",
                                  num_days_logged >= 20 ~ "Frequent Usage")) 

# Vix #6 - BAR CHART: How many participants are in each usage level?
# !!! KEY INSIGHT #6: 30 of the 33 participants used their devices for most of the 31 days
participants_usage_level <- daily_usage %>%   
  group_by(usage_level) %>% # groups by usage level
  summarize(num_participants = sum(n())) %>% # counts the number of participants in each usage level
  arrange(num_participants) # sorts by number of participants

head(participants_usage_level)

participants_usage_level$usage_level <- participants_usage_level$usage_level %>% 
  ordered(levels = c("No Usage",
                     "Minimal Usage",
                     "Moderate Usage",
                     "Frequent Usage")) # re-orders the usage levels for plot viz

ggplot(participants_usage_level, aes(x=usage_level, y=num_participants)) + # begins plot
  geom_bar(stat="identity", color = bellabeat_palette[1], fill = bellabeat_palette[3]) + # creates, colors and fills plot
  geom_text(aes(label = num_participants), 
            vjust = -.5, 
            fontface = "bold",
            color = bellabeat_palette[1]) + #adds, adjust, bolden, and colors number of participants labels above bars
  theme_light() + # desired viz theme
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + # centers and enlarges chart title
  labs(x = "Usage Level",
       y = "Number of Participants",
       title = "Distribution of Usage Levels") # chart labels

# DEEPER DIVE: Exploring what percentage of the day participants wore their device during the 31 days
# Viz #7 - PIE CHART: How often did participants wear their device as a percentage?
# !!! KEY INSIGHT #7: 51% of participants wore their device constantly (100% of the time) and 35% of participant wore it for most of the day (50-75%)
daily_activity_usage <- merge(daily_activity, daily_usage, by = c("id")) # mixing daily activity and daily usage together
head(daily_activity_usage)

minutes_device_worn <- daily_activity_usage %>% 
  mutate(total_minutes_worn = very_active_minutes + fairly_active_minutes + lightly_active_minutes + sedentary_minutes) %>% 
  mutate(device_worn_percentage = round((total_minutes_worn / 1440) * 100)) %>% 
  mutate(device_worn_level = case_when(device_worn_percentage < 25 ~ "Barely Worn (< 25%)",
                                       device_worn_percentage >= 25 & device_worn_percentage < 50 ~ "Somewhat Worn (25-50%)",
                                       device_worn_percentage >= 50 & device_worn_percentage < 75 ~ "Mostly Worn (50-75%)",
                                       device_worn_percentage >= 75 & device_worn_percentage < 100 ~ "Frequently Worn (75-99%)",
                                       device_worn_percentage == 100 ~ "Constantly Worn (100%)"))
head(minutes_device_worn)

minutes_device_worn_all_usage_percentage <- minutes_device_worn %>% 
  group_by(device_worn_level) %>% # groups by device wear level
  summarize(minutes = n()) %>% # counts the number of rows 
  mutate(total_minutes = sum(minutes)) %>% # sums the rows
  group_by(device_worn_level) %>% # groups again by device wear level
  summarize(total_percentage = minutes / total_minutes) %>% # obtains percentage of minutes
  mutate(percentage_labels = scales::percent(total_percentage)) # creates labels for plotting

minutes_device_worn_all_usage_percentage$device_worn_level <- minutes_device_worn_all_usage_percentage$device_worn_level %>% 
  factor(levels = c("Barely Worn (< 25%)",
                    "Somewhat Worn (25-50%)",
                    "Mostly Worn (50-75%)",
                    "Frequently Worn (75-99%)",
                    "Constantly Worn (100%)"))

# REASONING: 'Barely Worn and Somewhat Worn percentages were overlapping so need to use the midpoint of the pie for label positioning 
# determines position of labels
minutes_device_worn_all_usage_percentage$cumsum <- cumsum(minutes_device_worn_all_usage_percentage$total_percentage) 
# determines y position of the labels
minutes_device_worn_all_usage_percentage$midpoint <- minutes_device_worn_all_usage_percentage$cumsum - (minutes_device_worn_all_usage_percentage$total_percentage / 2)

# starts the plot of percentages device has been worn
ggplot(minutes_device_worn_all_usage_percentage, aes(x = "", y = total_percentage, fill = device_worn_level)) +
  # first part needed for pie chart, bar chart layer and ensuring that pre-calculated percentages are used
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + # second part needed for pie chart, y-values used for slices
  theme_void() + # uses void theme for aesthetic reasons
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        plot.subtitle = element_text(hjust = 0.5, size = 10)) + # centers title (subtitle_ and sets font size to 14 (10)
  # reverses and applies custom colors to pie slices (Constantly Worn = Darkest Tint and so forth)
  scale_fill_manual(values = rev(bellabeat_palette)) + 
  geom_label_repel(aes(y = midpoint,label = percentage_labels), # handles overlapping percentage labels
                   segment.color = NA, # removes label lanes
                   show.legend = FALSE) + # removes repel's legend (thus, removing the random "a" on legend)
  labs(x = NULL,
       y = NULL, # removes x- and y-axes titles
       fill = "Device Worn", # changes legend title
       title = "Time Worn per Day",# adds chart title
       subtitle = "% represents how often participants wore their device throughout the day") # adds chart subtitle





