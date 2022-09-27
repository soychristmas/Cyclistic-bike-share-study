m1_y2021 <- read_excel("202101-divvy-tripdata(AutoRecovered).xls")
m2_y2021 <- read_excel("202102-divvy-tripdata.xls")
m3_y2021 <- read_excel("202103-divvy-tripdata.xls")
m4_y2021 <- read_excel("202104-divvy-tripdata.xls")
m5_y2021 <- read_excel("202105-divvy-tripdata.xls")
m6_y2021 <- read_excel("202106-divvy-tripdata.xls")
m7_y2021 <- read_excel("202107-divvy-tripdata.xls")
m8_y2021 <- read_excel("202108-divvy-tripdata.xls")
m9_y2021 <- read_excel("202109-divvy-tripdata.xls")
m10_y2021 <- read_excel("202110-divvy-tripdata.xls")
m11_y2021 <- read_excel("202111-divvy-tripdata.xls")
m12_y2021 <- read_excel("202112-divvy-tripdata.xls")


str(m1_y2021)
str(m2_y2021)
str(m3_y2021)
str(m4_y2021)
str(m5_y2021)
str(m6_y2021)
str(m7_y2021)
str(m8_y2021)
str(m9_y2021)
str(m10_y2021)
str(m11_y2021)
str(m12_y2021)

all_2021 <- bind_rows(m1_y2021, m2_y2021, m3_y2021, m4_y2021, m5_y2021, m6_y2021, 
                      m7_y2021, m8_y2021, m9_y2021, m10_y2021, m11_y2021, m12_y2021)

all_2021 <- all_2021 %>%  
  select(-c(start_station_name, start_station_id, end_station_name, end_station_id, 
            start_lat, start_lng, end_lat, end_lng, sum_casu_class_sun, 
            sum_casu_elec_sun, average, mean_of_ridelength, ...19, ...20, ...21,
            max_ridelength, most_times_used))

all_2021$date <- as.Date(all_2021$started_at)
all_2021$month <- format(as.Date(all_2021$date), "%m")
all_2021$day <- format(as.Date(all_2021$date), "%d")
all_2021$year <- format(as.Date(all_2021$date), "%Y")
all_2021$day_of_week <- format(as.Date(all_2021$date), "%A")

all_2021_v2 <- all_2021[!(all_2021$ride_length<0),]

summary(all_2021_v2$ride_length)


aggregate(all_2021_v2$ride_length ~ all_2021_v2$member_casual, FUN = mean)
aggregate(all_2021_v2$ride_length ~ all_2021_v2$member_casual, FUN = median)
aggregate(all_2021_v2$ride_length ~ all_2021_v2$member_casual, FUN = max)
aggregate(all_2021_v2$ride_length ~ all_2021_v2$member_casual, FUN = min)

aggregate(all_2021_v2$ride_length ~ all_2021_v2$member_casual + 
            all_2021_v2$day_of_week + all_2021_v2$rideable_type, FUN = mean)

all_2021_v2$day_of_week <- ordered(all_2021_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", 
                                             "Wednesday", "Thursday", "Friday", 
                                             "Saturday"))

aggregate(all_2021_v2$ride_length ~ all_2021_v2$member_casual + 
            all_2021_v2$day_of_week + all_2021_v2$rideable_type, FUN = mean)

all_2021_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x = "Weekday", y = "Number of Rides", title = "Weekday vs. Number of Rides", fill = "Member/Casual")

all_2021_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x = "Weekday", y = "Average Duration", title = "Weekday vs. Average Duration", fill = "Member/Casual")

all_2021_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x = "Bike Type", y = "Average Duration", title = "Bike Type vs. Average Duration", fill = "Member/Casual")

all_2021_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday, rideable_type) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x = "Bike Type", y = "Number of Rides", title = "Bike Type vs. Number of Rides", fill = "Member/Casual")

