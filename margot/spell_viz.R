library(tidyverse)
library(anytime)
spells <- read_csv('spells.csv')
users <- read_csv('unique user ids with affiliation.csv')
spells <- left_join(spells, users, by="user")

se <- function(x) sqrt(var(x)/length(x))

#Without affiliations:

#Distribution across year

spells %>%
  group_by(location, date) %>%
  filter(maxstay < 50) %>%
  summarize(medstay = median(maxstay), meanstay = mean(maxstay)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = medstay), alpha = 0.5) + 
  geom_line(aes(x = date, y = meanstay, color = location)) + 
  facet_grid(rows = vars(location)) +
  xlab("Date") +
  ylab("Mean (color) and median (black) stay length") +
  ggtitle("Stay length over the year")

#Distribution across semester

#Spring

spells %>%
  group_by(location, date) %>%
  filter(maxstay < 50,
         date >= anydate("01/17/2018"),
         date <= anydate("05/11/2018")) %>%
  summarize(medstay = median(maxstay), meanstay = mean(maxstay)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = medstay), alpha = 0.5) + 
  geom_line(aes(x = date, y = meanstay, color = location)) + 
  facet_grid(rows = vars(location)) +
  xlab("Date") +
  ylab("Mean (color) and median (black) stay length") +
  ggtitle("Stay length over the spring semester")

#Fall

spells %>%
  group_by(location, date) %>%
  filter(maxstay < 50,
         date >= anydate("08/28/2018"),
         date <= anydate("12/18/2018")) %>%
  summarize(medstay = median(maxstay), meanstay = mean(maxstay)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = medstay), alpha = 0.5) + 
  geom_line(aes(x = date, y = meanstay, color = location)) + 
  facet_grid(rows = vars(location)) +
  xlab("Date") +
  ylab("Mean (color) and median (black) stay length") +
  ggtitle("Stay length over the fall semester")

#Distribution across week

spells %>%
  mutate(week = wday(time, week_start = 1)) %>%
  group_by(location, week) %>%
  filter(maxstay < 50) %>%
  summarize(medstay = median(maxstay), 
            meanstay = mean(maxstay),
            sdstay = sd(maxstay)) %>%
  ggplot() + 
  geom_line(aes(x = week, y = medstay, color = location), alpha = 0.5) + 
  geom_line(aes(x = week, y = meanstay, color = location)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")) + 
  xlab("Weekdays") +
  ylab("Mean (color) and median (light) stay length") +
  ggtitle("Stay length across an average week")

spells %>% 
  group_by(location, maxstay) %>%
  filter(maxstay < 10) %>%
  summarize(count = n()) %>%
  ggplot() + 
  geom_bar(aes(x = maxstay, y = count, fill = location), stat = "identity") + 
  facet_grid(cols = vars(location)) +
  xlab("Stay lengths") +
  ylab("Number of visits") +
  ggtitle("Distribution of stay lengths")

#Distribution of half hour visits

spells %>% 
  filter(maxstay == 0.5) %>%
  group_by(location, date) %>%
  summarize(count = n()) %>%
  ggplot() + 
  geom_bar(aes(x = date, y = count, fill = location), stat = "identity") + 
  facet_grid(rows = vars(location)) +
  xlab("Date") +
  ylab("Number of visits") +
  ggtitle("Distribution of half hour visits")

spells %>% 
  filter(maxstay == 0.5) %>%
  mutate(hour = hour(time)) %>%
  group_by(location, hour) %>%
  summarize(count = n()) %>%
  ggplot() + 
  geom_bar(aes(x = hour, y = count, fill = location), stat = "identity") + 
  facet_grid(rows = vars(location)) +
  scale_x_continuous(breaks=c(0,6,12,18,24),labels=c("00:00","06:00","12:00","18:00","24:00")) +
  xlab("Time of Day") +
  ylab("Number of visits") +
  ggtitle("Distribution of half hour visits")

#With affiliations:

spells %>% 
  group_by(location, affiliation) %>% 
  summarize(stay = mean(maxstay),
            stayse = se(maxstay)) %>%
  ggplot() +
  geom_bar(aes(x = affiliation, y = stay, fill = location), stat="identity", position="dodge") +
  geom_errorbar(aes(x = affiliation, ymin = stay-stayse, ymax = stay+stayse, group=location), position = "dodge") +
  xlab("Affiliation") +
  ylab("Stay length") +
  ggtitle("Distribution of stay length by affiliation")

spells %>%
  group_by(affiliation, location, date) %>%
  filter(maxstay < 50,
         date >= anydate("01/17/2018"),
         date <= anydate("05/11/2018")) %>%
  summarize(stayse = se(maxstay), meanstay = mean(maxstay)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = meanstay, color = location)) + 
  geom_ribbon(aes(x = date, ymin = meanstay-stayse, ymax = meanstay+stayse, fill = location), alpha = 0.5) +
  facet_grid(rows = vars(affiliation), scales="free") +
  xlab("Date") +
  ylab("Mean stay length with standard error") +
  ggtitle("Stay length over the spring semester")

spells %>%
  group_by(affiliation, location, date) %>%
  filter(maxstay < 50,
         date >= anydate("08/28/2018"),
         date <= anydate("12/18/2018")) %>%
  summarize(stayse = se(maxstay), meanstay = mean(maxstay)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = meanstay, color = location)) + 
  geom_ribbon(aes(x = date, ymin = meanstay-stayse, ymax = meanstay+stayse, fill = location), alpha = 0.5) +
  facet_grid(rows = vars(affiliation), scales="free") +
  xlab("Date") +
  ylab("Mean stay length with standard error") +
  ggtitle("Stay length over the fall semester")





spells %>% 
  group_by(location, affiliation, user) %>% 
  summarize(visits = n()) %>%
  summarize(avgvisits = mean(visits)) %>%
  ggplot() +
  geom_bar(aes(x = affiliation, y = avgvisits, fill = location), stat="identity", position="dodge")

spells %>% 
  group_by(location, affiliation, user) %>% 
  summarize(stay = mean(maxstay)) %>%
  summarize(avgstay = mean(stay)) %>%
  ggplot() +
  geom_bar(aes(x = affiliation, y = avgstay, fill = location), stat="identity", position="dodge")

spells %>%
  filter(location == "clemons",
         affiliation == "student" | affiliation == "faculty") %>%
  group_by(affiliation, date) %>%
  summarize(stay = mean(maxstay),
            stayse = se(maxstay)) %>%
  filter(stay < 50) %>%
  ggplot() + 
  geom_line(aes(x = date, y = stay, color = affiliation)) +
  geom_ribbon(aes(x = date, ymin = stay-stayse, ymax = stay+stayse, fill = affiliation), alpha = 0.25)





