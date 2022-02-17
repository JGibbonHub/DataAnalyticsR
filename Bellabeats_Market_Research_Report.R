# Set up working environment 

library(tidyverse)
library(janitor)
library(lubridate)
library(data.table)
library(plyr)
library(zoo)
library(lookup)
library(colorBlindness)
setwd("C:/Users/pc/Documents/GitHub/DataAnalyticsR/DataAnalyticsR")


# Import data from csv to data frames

activity_daily <- read.csv(file = "dailyActivity_merged.csv", header = TRUE, sep = ",")
heartrate_seconds <- read.csv(file = "heartrate_seconds_merged.csv", header = TRUE, sep = ",")
mets_minute <- read.csv(file = "minuteMETsNarrow_merged.csv", header = TRUE, sep = ",")
sleep_daily <- read.csv(file = "sleepDay_merged.csv", header = TRUE, sep = ",")
sleep_minute <- read.csv(file = "minuteSleep_merged.csv", header = TRUE, sep = ",")

# View Data to ensure import was successful. The focus will be on the daily data with the exception of the heart rate and sleep data. 

tibble(activity_daily)
tibble(heartrate_seconds)
tibble(mets_minute)
tibble(sleep_daily)
tibble(sleep_minute)

# Cleaning process with janitor

# Based on the results the column names for date and time to be made consistent

activity_daily_cln <- activity_daily %>% 
  rename(c("ActivityDate" = "date")) %>% 
  clean_names()

# Heart rate data column to be changed from time to date time

heartrate_seconds_cln <- heartrate_seconds %>% 
  rename(c("Time" = "date_time")) %>% 
  clean_names()

# METS data column name change from activity minutes to date time 

mets_minute_cln <- mets_minute %>% 
  rename(c("ActivityMinute" = "date_time", "METs" = "mets")) %>% 
  clean_names()

# Sleep data column names to be changed the SleepDay to Date

sleep_daily_cln <- sleep_daily %>% 
  rename(c("SleepDay" = "date")) %>% 
  clean_names()

# Sleep minute column name date to date time

sleep_minute_cln <- sleep_minute %>% 
  rename(c("date" = "date_time")) %>% 
  clean_names()

# Produce a table summarizing the table based on id.

tabyl(activity_daily_cln, id)

# The outcome is there was only 21 people that wore the fitness device for 31 days

tabyl(activity_daily_cln, id) %>% 
  summarise(count(n))

# The heart rate was more incomplete that the activity data with only 14 participants 

tabyl(heartrate_seconds_cln,id) %>% 
  summarise(count(n))

# There was 33 observations in total of varying lengths

tabyl(mets_minute_cln, id) %>% 
  summarise(count(n))

# There were 24 participants and tracking there sleep a minimal of 1 and a maximum of 32 days.

tabyl(sleep_daily_cln, id) %>% 
  summarise(count(n))

# There were 24 participants and tracking there sleep a minimal of 69 min and a maximum of 15682 min. This is consistent with the observations noted above.

tabyl(sleep_minute_cln, id) %>% 
  summarise(count(n))

# To be able to plot the data the date formats of all the dataframes needs to be changed

# Date is changed for the two dataframes below

activity_daily_cln_v2 <- print(activity_daily_cln)
activity_daily_cln_v2[[2]] <- as.Date(activity_daily_cln[[2]], "%m/%d/%Y")
tibble(activity_daily_cln_v2)

sleep_daily_cln_v2 <- print(sleep_daily_cln)
sleep_daily_cln_v2[[2]] <- as.Date(sleep_daily_cln[[2]], "%m/%d/%Y")
tibble(sleep_daily_cln_v2)

# The dataframes below needs datetime format

heartrate_seconds_cln_v2 <- print(heartrate_seconds_cln)
heartrate_seconds_cln_v2[[2]] <- as.POSIXct(heartrate_seconds_cln[[2]], format = "%m/%d/%Y %H:%M:%S %p")
tibble(heartrate_seconds_cln_v2)

mets_minute_cln_v2 <- print(mets_minute_cln)
mets_minute_cln_v2[[2]] <- as.POSIXct(mets_minute_cln[[2]], format = "%m/%d/%Y %H:%M:%S %p")
tibble(mets_minute_cln_v2)

# Separating column for the date time in tables for heart rate and mets

heartrate_seconds_cln_v3 <- separate(heartrate_seconds_cln_v2, col = date_time, c("date","time"), sep = " ")
heartrate_seconds_cln_v3[[2]] <- as.Date(heartrate_seconds_cln_v3[[2]], "%Y-%m-%d")
tibble(mets_minute_cln_v3)

mets_minute_cln_v3 <- separate(mets_minute_cln_v2, col = date_time, c("date","time"), sep = " ")
mets_minute_cln_v3[[2]] <- as.Date(mets_minute_cln_v3[[2]], "%Y-%m-%d")
tibble(mets_minute_cln_v3)

# Looking for any duplication or missing values

# No duplicates 
activity_daily_cln_v2 %>% 
  get_dupes() %>% 
  summarise(count(id))


# Multiple duplicates  
heartrate_seconds_cln_v3 %>% 
  get_dupes() %>% 
  summarise(count(id))

# Remove the duplicates from heart rate data

heartrate_seconds_cln_v4 <- heartrate_seconds_cln_v3 %>% 
  distinct()

# Duplicates observed and thus needs to be removed
mets_minute_cln_v3 %>% 
  get_dupes() %>% 
  summarise(count(id))

# Remove duplicates from mets data

mets_minute_cln_v4 <- mets_minute_cln_v3 %>% 
  distinct() 

# Duplicates observed in the data frame below

sleep_daily_cln_v2 %>% 
  get_dupes() %>% 
  summarise(count(id))

# Remove duplicates from sleep data

sleep_daily_cln_v3 <- sleep_daily_cln_v2 %>% 
  distinct()

# Final step of the cleaning process is to summarize the min and sec data into daily data in order to use for the daily insights.

heartrate_seconds_cln_dt <- data.table(heartrate_seconds_cln_v4)
heartrate_seconds_cln_dt_2 <- heartrate_seconds_cln_dt[,list(mean_hrt = mean(value), max_hrt = max(value), min_hrt = min(value)), by = c("id,date")]


mets_minute_cln_dt <- data.table(mets_minute_cln_v4)
mets_minute_cln_dt_2 <- mets_minute_cln_dt[,list(mean_mets = mean(mets), max_mets = max(mets), min_mets = min(mets)), by = c("id,date")]


# Using the summary function you can quickly identify if there are unusual numbers  
heartrate_seconds_cln_dt_2 %>% 
  summary()

mets_minute_cln_dt_2 %>% 
  summary()

activity_daily_cln_v2 %>% 
  summary()

sleep_daily_cln_v3 %>% 
  summary()

# Analyze phase of the analysis 

# 1. Starting with descriptive statistics

activity_daily_cln_dt <- data.table(activity_daily_cln_v2)

activity_daily_cln_dt_2 <- activity_daily_cln_dt[,list(total_active_minutes = sum(very_active_minutes, fairly_active_minutes, lightly_active_minutes)), by = c("id,date")]

activity_daily_cln_dt_3 <- activity_daily_cln_dt %>% 
  inner_join(activity_daily_cln_dt_2)

activity_daily_cln_dt_3$weekday <- weekdays(activity_daily_cln_dt_2$date)

activity_daily_mets_cln_merge <- activity_daily_cln_dt_3 %>% 
  inner_join(mets_minute_cln_dt_2)

tibble(activity_daily_mets_cln_merge)

activity_daily_mets_summary_stats <- activity_daily_mets_cln_merge %>% 
  select(total_steps, total_distance, tracker_distance, logged_activities_distance, very_active_distance, moderately_active_distance, light_active_distance, sedentary_active_distance, very_active_minutes, fairly_active_minutes, lightly_active_minutes, sedentary_minutes, calories, total_active_minutes) %>% 
  summary()

# 2. Descriptive statistics for sleep data 

tibble(sleep_daily_cln_v3)

sleep_daily_cln_dt <- data.table(sleep_daily_cln_v3)

sleep_daily_cln_dt_2 <- sleep_daily_cln_dt[,list(mean_minutes_asleep = mean(total_minutes_asleep), mean_minutes_in_bed = mean(total_time_in_bed)), by = c("id")]

sleep_daily_cln_dt_3 <- sleep_daily_cln_dt_2[,list(mean_minutes_not_sleeping = mean_minutes_in_bed - mean_minutes_asleep, mean_hour_asleep = mean_minutes_asleep / 60, mean_hour_in_bed = mean_minutes_in_bed / 60), by = c("id")]

sleep_daily_cln_dt_4 <- sleep_daily_cln_dt_2 %>% 
  inner_join(sleep_daily_cln_dt_3)

# ID 1844505072 is suspicious as the hour spend in bed is 16 hour which is unusual. As for id 2320127002 spending 1 hour asleep seems unusual.
tibble(sleep_daily_cln_dt_4)


sleep_daily_personal_summary_stats <- sleep_daily_cln_v3 %>% 
  select(total_sleep_records, total_minutes_asleep, total_time_in_bed) %>% 
  summary()

sleep_daily_summary_stats <- sleep_daily_cln_dt_4 %>% 
  select(mean_minutes_asleep, mean_minutes_in_bed, mean_minutes_not_sleeping, mean_hour_asleep, mean_hour_in_bed) %>% 
  summary()


# Correlations found the negative correlation between sedentary min and calories which is expected, although the strong correlation between very active min and calories is an interesting observation
correlation_activity <- activity_daily_mets_cln_merge %>% 
  summarise(cor(total_active_minutes,calories), cor(very_active_minutes,calories), cor(fairly_active_minutes,calories), cor(lightly_active_minutes,calories), cor(sedentary_minutes,calories), cor(mean_mets,calories), cor(max_mets,calories), cor(min_mets, calories))

weekday_total_act_min <- activity_daily_cln_dt_3[,list(weekday_mean_total_act_min = mean(total_active_minutes), weekday_max_total_act_min = max(total_active_minutes), weekday_min_total_act_min = min(total_active_minutes)), by = c("weekday")] %>% 
  arrange(weekday_mean_total_act_min)

weekday_act_min_summary_stats <- print(weekday_total_act_min)


# Share data visuals 

# Plotting the total activity minutes by weekday.

ggplot(weekday_act_min_summary_stats, aes(x = factor(weekday,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")), y = round(weekday_mean_total_act_min, digits = 0), fill = weekday, label = round(weekday_mean_total_act_min, digits = 0))) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Activity by day of the week", subtitle = "Data from 12-Apr-16 to 12-May-16", x = "", y = "Total Activity Minutes", color = "black") + 
  geom_text(color = "black", vjust = 1.5) + 
  scale_fill_manual(values = Blue2Orange10Steps)

# Plotting correlations between very active minutes and calories.

ggplot(activity_daily_mets_cln_merge, aes(x = very_active_minutes, y = calories, color = calories)) +
  geom_point() +
  geom_smooth(method = lm, level = 0.99) +
  labs(title = "Very Active Minutes vs Calories", subtitle = "Data from 12-Apr-16 to 12-May-16", x = "Very Active Minutes", y = "Calories")

# Total Active Minutes comparison to calories.

ggplot(activity_daily_mets_cln_merge, aes(x = total_active_minutes, y = calories, color = calories)) +
  geom_point() +
  geom_smooth(method = lm, level = 0.99) +
  labs(title = "Total Active Minutes vs Calories", subtitle = "Data from 12-Apr-16 to 12-May-16", x = "Total Active Minutes", y = "Calories")

# Mean METs correlation to calories burned

ggplot(activity_daily_mets_cln_merge, aes(x = mean_mets, y = calories, color = calories)) +
  geom_point() +
  geom_smooth(method = lm, level = 0.99) +
  labs(title = "Mean METs vs Calories", subtitle = "Data from 12-Apr-16 to 12-May-16", x = "Mean METs", y = "Calories")

# Plotting Sleep data 

ggplot(sleep_daily_cln_dt_4, aes(x = substr(id, 1, 3), y = mean_hour_asleep, fill = mean_hour_asleep, width = 0.5)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_hline(yintercept = c(7,9), size = 1.5) +
  geom_hline(yintercept = mean(sleep_daily_cln_dt_4$mean_hour_asleep), color = "green", size = 2, label = round(mean(sleep_daily_cln_dt_4$mean_hour_asleep), digits = 1)) +
  labs(title = "Mean Sleep Data", subtitle = "Data from 12-Apr-16 to 12-May-16", x = "Short ID", y = "Hours of Sleep", fill = "Mean Hours Asleep") +
  geom_label(aes(0, round(mean(sleep_daily_cln_dt_4$mean_hour_asleep),digits = 1), label = round(mean(sleep_daily_cln_dt_4$mean_hour_asleep), digits = 1), hjust = 0, vjust = 1.25)) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11))


# Plotting total step vs Calories

ggplot(activity_daily_mets_cln_merge, aes(x = total_steps, y = calories, color = total_steps)) +
  geom_point() +
  geom_smooth(method = lm, level = 0.99) +
  labs(title = "Total Steps vs Calories", subtitle = "Data from 12-Apr-16 to 12-May-16", x = "Total Steps", y = "Calories", color = "Total Daily Steps")





