# CASE STUDY: Cyclistic-bike-share

Author: Jocelyn Crawford

Date: December 24, 2021

This case study follow the six phases of analysis:

Ask

Prepare

Process

Analyze

Share 
 
 Act




# Scenerio
In 2016, Cyclistic launched a successful bike-share offering in Chicago. The company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members.

# Ask

Buisness Task: Analyze rider data to design marketing strategies aimed at converting casual riders into annual members.

Primary Stakeholders: Cyclistic executive team and Lily Moreno

Secondary Stakeholders: Cyclistic marketing analytics team

# Prepare
Data Source: 12 months (Apr.2019-Mar.2020)of Cyclistic trip Data from Motivate International Inc: [data source link](https://divvy-tripdata.s3.amazonaws.com/index.html) and [license](https://ride.divvybikes.com/data-license-agreement)
The dataset has 12 CSV, 13 columns, and 3.2M rows. The data also follow a ROCCC approach:

~Reliability:the data includes complete and accurate ride data from Divvy

~Original:the data is from Motivate International Inc

~Comprehensive: the data includes types of bikes,start and end station name, start and end time, station ID, ride type,and  membership types.

~Current: data is up to date to Aug 2020

~Cited: This data is cited and under current [license](https://ride.divvybikes.com/data-license-agreement) agreement 

This dataset has limitations:

Personally identifiable information: the dataset has a restriction of personally identifiable information, . This
means that you won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the
Cyclistic service area or if they have purchased multiple single passes

NA Values : after checking sum(is.na(all_trips_v2)),found that the daset has 27573585 values.

# Process
#wrangle and combine data

colnames(q3_2019)

colnames(q4_2019)

colnames(q2_2019)

colnames(q1_2020)

#examine data 
colnames(all_trips)

nrow(all_trips)

dim(all_trips)

head(all_trips)

str(all_trips)

summary(all_trips)

tail(all_trips

#rename columns
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))


#Stacking
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


#add columns

all_trips$date <- as.Date(all_trips$started_at)

all_trips$month <- format(as.Date(all_trips$date), "%m")

all_trips$day <- format(as.Date(all_trips$date), "%d")

all_trips$year <- format(as.Date(all_trips$date), "%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#add ride_length calcualtions
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at,units = "secs")

#remove "bad" data and NA values
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

all_trips_v3 <- na.omit(all_trips_v2)


#clean data to prepare for analysis in next steps !

# Analyze

Check min, max, mean, median and any outlier on the ride length.( In seconnds)

summary(all_trips_v3$ride_length)

#Compare members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#See average ride time by each day for members vs casual users

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

#dates in order

all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#run average ride time by each day for members vs casual users

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

#analyze ridership data by type and weekday

all_trips_v3 %>%  
  mutate(weekday = wday(started_at,label = TRUE)) %>%    #creates weekday field using wday()
  group_by(member_casual, weekday) %>%   #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%  		# calculates the average duration
  arrange(member_casual, weekday)		

#visualize the number of rides by rider type

all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
 group_by(member_casual, weekday) %>%   summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Let's create a visualization for average duration

all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

##Create a visualization for total number of rides by month  

all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
 
 for complete R script click [here](https://github.com/Jocebc/Cyclistic-bike-share/blob/69c26b9c81940cf50ba566f7b2fe64683f8d81da/Google%20Data%20Analytics%20Cyclistics%20Case%20Study) to go to link 
