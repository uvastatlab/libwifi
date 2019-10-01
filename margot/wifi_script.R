library(tidyverse)
library(lubridate)
library(anytime)

se = function(x) sqrt(var(x)/length(x))

wifi <- read_rds('wifi.rds')
gate <- read.csv('2018gate-counts.csv')

#Full half hour time series
by_half <- wifi %>% 
  group_by(location,time) %>% 
  summarize(uniq = length(unique(user)))
by_half %>% 
  ggplot() + 
  geom_line(aes(x = time, y = uniq, color = location), alpha = 0.5)

#Half hour averages with error
by_half_avg <- by_half %>% 
  mutate(daytime = hour(time) + (minute(time)/60)) %>%
  ungroup() %>%
  group_by(location,daytime) %>%
  summarize(avg = mean(uniq),
            serr = se(uniq))

by_half_avg %>% ggplot(aes(x = daytime, y = avg)) + 
  geom_line(aes(color=location)) +
  geom_ribbon(aes(ymin = avg-serr, ymax = avg+serr, fill = location), alpha = 0.25)

#Averages with standard deviation only while school is in session
by_half_semester <- by_half %>%
  filter(!(time < anytime("2018-03-01")),
         !(time > anytime("2018-05-11") & time < anytime("2018-08-28")),
         !(time > anytime("2018-12-18")))

by_half_semester %>% 
  mutate(daytime = hour(time) + (minute(time)/60)) %>%
  ungroup() %>%
  group_by(location,daytime) %>%
  summarize(avg = mean(uniq),
            std = sd(uniq)) %>%
  ggplot(aes(x = daytime, y = avg)) + 
  geom_line(aes(color=location)) +
  geom_ribbon(aes(ymin = avg-std, ymax = avg+std, fill = location), alpha = 0.25) +
  xlab("Time (Hours)") +
  ylab("Average unique users") +
  ggtitle("Average unique users with STD during semesters")

#Convert date column of gate count to datetimes and standardize location names
gate <- gate %>%
  mutate(dtm = anydate(parse_date_time(date,'%m/%d/%y %H:%M')),
         month = month(dtm, label=TRUE))
levels(gate$name) = c("alderman","clemons","harrison")
gate$location = gate$name
gate <- select(gate, -c(name,date,X))

#Get unique users by day
by_day <- wifi %>% 
  mutate(dtm = anydate(time)) %>%
  group_by(location,dtm) %>%
  summarize(uniq = length(unique(user)))
  
#Join wifi data with gate count by day
joint_day <- left_join(by_day, gate, by=c("dtm","location"))

joint_day %>% 
  filter(location == "alderman") %>%
  ggplot(aes(x = dtm)) + 
  geom_line(aes(y = uniq)) + 
  geom_line(aes(y = visitors, color="red")) +
  xlab("Days") +
  ylab("Unique wifi users (gate visitors in red)") +
  ggtitle("Unique wifi visitors and gate count visitors by day at Alderman")

joint_day %>% 
  filter(location == "clemons") %>%
  ggplot(aes(x = dtm)) + 
  geom_line(aes(y = uniq)) + 
  geom_line(aes(y = visitors, color="red")) +
  xlab("Days") +
  ylab("Unique wifi users (gate visitors in red)") +
  ggtitle("Unique wifi visitors and gate count visitors by day at Clemons")

joint_day %>% 
  filter(location == "harrison") %>%
  ggplot(aes(x = dtm)) + 
  geom_line(aes(y = uniq)) + 
  geom_line(aes(y = visitors, color="red")) +
  xlab("Days") +
  ylab("Unique wifi users (gate visitors in red)") +
  ggtitle("Unique wifi visitors and gate count visitors by day at Harrison")

#Get unique users by month
by_month <- wifi %>% 
  mutate(dtm = anydate(time),
         month = month(dtm, label=TRUE)) %>%
  group_by(location,month) %>%
  summarize(uniq = length(unique(user)))

#Join wifi data with gate count by month
gate_month <- gate %>%
  group_by(location, month) %>%
  summarize(gate_count = sum(visitors))

joint_month <- left_join(by_month, gate_month, by=c("month","location"))

joint_month %>%
  ggplot(aes(x = month(anydate(month)), y = uniq, color = location)) + 
  geom_line() + 
  geom_line(aes(y = gate_count), linetype=2) +
  xlab("Months") +
  ylab("Unique wifi users (gate visitors in red)") +
  ggtitle("Monthly wifi visitors and gate count")
         