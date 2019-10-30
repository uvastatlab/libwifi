library(tidyverse)
library(lubridate)
library(anytime)

#join information about library hours and fixed devices
wifi <- read_rds('wifi.rds')
openhours <- read_rds('clem_ald_hours.rds')
fixed <- read.csv('fixeddevicesunionholiday.csv')

fixed <- fixed %>% select(user) %>% mutate(fix = 1)

wifi_closed <- left_join(wifi, openhours, by = "time")
wifi_closed <- left_join(wifi_closed, fixed, by = "user")
wifi_closed$fix <- ifelse(is.na(wifi_closed$fix), 0, wifi_closed$fix)
wifi_closed <- wifi_closed %>%
  filter(location != "harrison") %>%
  mutate(open = if_else(location=="alderman", alderman, clemons)) %>%
  select(-c(alderman, clemons))

#Percentage open for Alderman (red) and Clemons (black)
openhours %>%
  group_by(hour) %>%
  summarize(open_ald = sum(alderman)/length(alderman),
            open_clem = sum(clemons)/length(clemons)) %>%
  ggplot() +
  geom_line(aes(x = hour, y = open_ald), color = "red") +
  geom_line(aes(x = hour, y = open_clem), color = "black") +
  scale_x_continuous(breaks=c(0,6,12,18,24),labels=c("00:00","06:00","12:00","18:00","24:00")) +
  xlab("Time (hours)") +
  ylab("% Open Throughout Year") +
  ggtitle("Open Percentage for Alderman (red) and Clemons (black)")

#Unique users throughout the year while libraries are closed
wifi_closed %>% 
  filter(fix == 0, open == 0) %>%
  group_by(location, date) %>%
  summarize(uniq = length(unique(user))) %>%
  ggplot() +
  geom_line(aes(x = date, y = uniq, color = location)) +
  xlab("Date") +
  ylab("Unique Users") +
  ggtitle("Unique users per day during library closures")

#Average unique users by hour during library closures per hour
wifi_closed %>% 
  filter(fix == 0, open == 0) %>%
  group_by(location, time) %>%
  summarize(uniq = length(unique(user))) %>%
  mutate(hour = hour(time)) %>%
  ungroup() %>%
  group_by(location, hour) %>%
  summarize(uniqavg = mean(uniq),
            uniqsd = sd(uniq)) %>%
  ggplot() +
  geom_line(aes(x = hour, y = uniqavg, color = location)) +
  geom_ribbon(aes(x = hour, 
                  ymin = uniqavg-uniqsd, ymax = uniqavg+uniqsd, 
                  fill = location), alpha = 0.25) + 
  scale_x_continuous(breaks=c(0,6,12,18,24),labels=c("00:00","06:00","12:00","18:00","24:00")) +
  xlab("Time (hours)") +
  ylab("Average Unique Users") +
  ggtitle("Average unique users by hour during library closures")

#Unique users across an average week during library closures
wifi_closed %>% 
  filter(fix == 0, open == 0) %>%
  group_by(location, time) %>%
  summarize(uniq = length(unique(user))) %>%
  mutate(hour = hour(time), minute = minute(time), day = wday(time, week_start = 1)) %>%
  ungroup() %>%
  group_by(location, day, hour, minute) %>%
  summarize(uniqavg = mean(uniq),
            uniqsd = sd(uniq)) %>%
  mutate(week = day + (hour/24) + (minute/24/60)) %>%
  ggplot() +
  geom_line(aes(x = week, y = uniqavg, color = location)) +
  geom_ribbon(aes(x = week, 
                  ymin = uniqavg-uniqsd, ymax = uniqavg+uniqsd, 
                  fill = location), alpha = 0.25) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")) + 
  xlab("Weekdays") + 
  ylab("Average unique users") +
  ggtitle("Unique users across an average week during library closures")

#Sample numbers for Alderman closures
openhours %>%
  filter(alderman == 0) %>%
  mutate(hour = hour(time), minute = minute(time), day = wday(time, week_start = 1)) %>%
  group_by(day, hour, minute) %>%
  summarize(closed_periods = n()) %>%
  mutate(week = day + (hour/24) + (minute/24/60)) %>% 
  ggplot() +
  geom_line(aes(x = week, y = closed_periods)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                   labels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")) + 
  xlab("Weekdays") +
  ylab("Number of closures") +
  ggtitle("Number of Alderman closures by weekday across the year")
  
#Sample numbers for Clemons closures
openhours %>%
  filter(clemons == 0) %>%
  mutate(hour = hour(time), minute = minute(time), day = wday(time, week_start = 1)) %>%
  group_by(day, hour, minute) %>%
  summarize(closed_periods = n()) %>%
  mutate(week = day + (hour/24) + (minute/24/60)) %>% 
  ggplot() +
  geom_line(aes(x = week, y = closed_periods)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")) +
  scale_y_continuous(limits = c(0,50)) + 
  xlab("Weekdays") +
  ylab("Number of closures") +
  ggtitle("Number of Clemons closures by weekday across the year")

#Presumed walkby data through the week during the semester and during break
walkby <- wifi_closed %>% 
  filter(fix == 0, open == 0, location == "alderman") %>%
  group_by(location, time) %>%
  summarize(uniq = length(unique(user))) %>%
  mutate(hour = hour(time), 
         minute = minute(time), 
         day = wday(time, week_start = 1),
         week = day + (hour/24) + (minute/24/60),
         month = month(time),
         semester = if_else(time > anydate("2018-08-28") & time < anydate("2018-12-18"), 1, 0),
         semester = if_else(time > anydate("2018-01-17") & time < anydate("2018-05-11"), 1, semester))

walkby %>%
  filter(semester == 0) %>%
  ggplot() +
  geom_point(aes(x = week, y = uniq, color = semester, group = )) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")) + 
  xlab("Weekdays") + 
  ylab("Average unique users") +
  ggtitle("Unique users across an average week during library closures")

#Estimate of walkbys by hour during a weekday
weekday_walkby <- walkby %>%
  filter(semester == 0, week < 5) %>%
  group_by(hour) %>%
  summarize(avguniq = mean(uniq))

weekday_walkby %>%
  ggplot() +
  geom_line(aes(x = hour, y = avguniq)) +
  scale_x_continuous(breaks=c(0,6,12,18,24),labels=c("00:00","06:00","12:00","18:00","24:00")) +
  xlab("Time (hours)") +
  ylab("Average walkbys") +
  ggtitle("Walkbys per hour during weekday")
  




