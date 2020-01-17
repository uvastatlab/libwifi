# Library closure
library(tidyverse)
library(lubridate)

wifi <- readRDS("WiFi_data/wifi.rds")

open <- as.data.frame(unique(wifi$time))
names(open) <- c("time")
open <- open %>% 
  mutate(clemons = 1,
         # alderman = 1,
         day = wday(time, label = TRUE),
         hour = hour(time),
         date = date(time)) 


# Clemons standard hours ----
#   00:00 Friday to 9:00 Saturday
#   00:00 Saturday to 10:00 Sunday
open <- open %>% 
  mutate(clemons = if_else(day == "Fri" & hour < 9, 0, clemons),
         clemons = if_else(day == "Sat" & hour < 10, 0, clemons))


# Clemons nonstandard hours ----
# excluding snow closures...

# Winter break/Intersession: 
#   Mon 1/1-closed; Tue 1/2-Thu 1/4 9-8, Fri 1/5 9-6, Sat 1/6 1-5, Sun 1/7 1-8; 
#   Mon 1/8-Thu 1/11 9-8, Fri 1/12 9-6, Sat 1/13 1-5, Sun 1/14 closed; Mon 1/15 9-6; Tue 1/16 9-6
open <- open %>% 
  mutate(clemons = if_else(date == as.Date("2018-01-01"), 0, clemons),
         clemons = if_else(between(date, as.Date("2018-01-02"), as.Date("2018-01-04")) & hour < 9, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-01-02"), as.Date("2018-01-04")) & hour > 20, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-05") & hour < 9 | date == as.Date("2018-01-05") & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-06") & hour < 13 | date == as.Date("2018-01-06") & hour > 17, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-07") & hour < 13 | date == as.Date("2018-01-07") & hour > 20, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-01-08"), as.Date("2018-01-11")) & hour < 9, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-01-08"), as.Date("2018-01-11")) & hour > 20, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-12") & hour < 9 | date == as.Date("2018-01-12") & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-13") & hour < 13 | date == as.Date("2018-01-13") & hour > 17, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-14"), 0, clemons),
         clemons = if_else(date == as.Date("2018-01-15") & hour < 9 | date == as.Date("2018-01-15") & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-01-16") & hour < 9 | date == as.Date("2018-01-16") & hour > 18, 0, clemons))

# Spring break: 
#   Fri 3/2 close at 6, Sat 3/3-Sun 3/4 1-5; Mon 3/5-Thu 3/8 8-10; Fri 3/9 8-6; Sat 3/10 1-5; Sun 3/11 10 to open
open <- open %>% 
  mutate(clemons = if_else(date == as.Date("2018-03-02") & hour < 18, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-03-03"), as.Date("2018-03-04")) & hour < 13, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-03-03"), as.Date("2018-03-04")) & hour > 17, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-03-05"), as.Date("2018-03-08")) & hour < 8, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-03-05"), as.Date("2018-03-08")) & hour > 22, 0, clemons),
         clemons = if_else(date == as.Date("2018-03-09") & hour < 8 | date == as.Date("2018-03-09") & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-03-10") & hour < 13 | date == as.Date("2018-03-10") & hour > 17, 0, clemons),
         clemons = if_else(date == as.Date("2018-03-11") & hour < 10, 0, clemons))

# Spring exams: 
#   Thu 5/3 - Thu 5/10 24 hours, Fri 5/11 close at 6; Sat 5/12 and Sun 5/13 1-5
open <- open %>% 
  mutate(clemons = if_else(between(date, as.Date("2018-05-03"), as.Date("2018-05-10")), 1, clemons),
         clemons = if_else(date == as.Date("2018-05-11") & hour > 18, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-05-12"), as.Date("2018-05-13")) & hour < 13, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-05-12"), as.Date("2018-05-13")) & hour > 17, 0, clemons))

# Clemons closed May 14, reopened August 10
open <- open %>% 
  mutate(clemons = if_else(between(date, as.Date("2018-05-14"), as.Date("2018-08-09")), 0, clemons))

# Summer hours: 
#   Fri, 8/10 9-6; Sat, 8/11 & 8/18 1-5; Sun 8/12 & 8/19 closed; Mon 8/13-Fri 8/17 & 8/20-8/24 9-6;
open <- open %>% 
  mutate(clemons = if_else(date == as.Date("2018-08-10") & hour < 9 | date == as.Date("2018-08-10") & hour > 18, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-18")) & hour < 13, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-18")) & hour > 17, 0, clemons),
         clemons = if_else(date == as.Date("2018-08-12") | date == as.Date("2018-08-19"), 0, clemons),
         clemons = if_else(between(date, as.Date("2018-08-13"), as.Date("2018-08-17")) & hour < 9, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-08-13"), as.Date("2018-08-17")) & hour > 18, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-08-20"), as.Date("2018-08-24")) & hour < 9, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-08-20"), as.Date("2018-08-24")) & hour > 18, 0, clemons))

# Move in: 
#   Sat 8/25 9-6, Sun 8/26 1-6
open <- open %>% 
  mutate(clemons = if_else(date == as.Date("2018-08-25") & hour < 9 | date == as.Date("2018-08-25") & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-08-26") & hour < 13 | date == as.Date("2018-08-26") & hour > 18, 0, clemons))

# Fall Break: 
#   Fri, 10/5 close at 6; Sat 10/6 1-5; Sun 10/7 1-5, Mon, 10/8 8-10; Tue, 10/9 8-24 hours
open <- open %>% 
  mutate(clemons = if_else(date == as.Date("2018-10-05") & hour > 18, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-10-06"), as.Date("2018-10-07")) & hour < 13, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-10-06"), as.Date("2018-10-07")) & hour > 17, 0, clemons),
         clemons = if_else(date == as.Date("2018-10-08") & hour < 8 | date == as.Date("2018-10-08") & hour > 22, 0, clemons),
         clemons = if_else(date == as.Date("2018-10-09") & hour < 8, 0, clemons))

# Thanksgiving: 
#   Tue, 11/20 close at 8; Wed, 11/21 8-6; Thu, 11/22 closed; Fri, 11/23 closed; Sat, 11/24 1-5; Sun, 11/25 10 to open
open <- open %>% 
  mutate(clemons = if_else(date == as.Date("2018-11-20") & hour > 20, 0, clemons),
         clemons = if_else(date == as.Date("2018-11-21") & hour < 8 | date == as.Date("2018-11-21") & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-11-22") | date == as.Date("2018-11-23"), 0, clemons),
         clemons = if_else(date == as.Date("2018-11-24") & hour < 13 | date == as.Date("2018-11-24") & hour > 17, 0, clemons),
         clemons = if_else(date == as.Date("2018-11-25") & hour < 10, 0, clemons))

# Fall Exams: 
#   Sat 12/8 to Mon 12/17 Clemons is 24 hours; Tue 12/18 close at 6; Sat 12/22 1-5; (what about Wed 12/19, Thu 12/20, Fri, 12/21? assume 9-6)
open <- open %>% 
  mutate(clemons = if_else(between(date, as.Date("2018-12-08"), as.Date("2018-12-17")), 1, clemons),
         clemons = if_else(date == as.Date("2018-12-18") & hour > 18, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-12-19"), as.Date("2018-12-21")) & hour < 9, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-12-19"), as.Date("2018-12-21")) & hour > 18, 0, clemons),
         clemons = if_else(date == as.Date("2018-12-22") & hour < 13 | date == as.Date("2018-12-22") & hour > 17, 0, clemons),
         clemons = if_else(between(date, as.Date("2018-12-22"), as.Date("2018-12-31")), 0, clemons))

# check
table(open$clemons)
open %>% group_by(day, hour) %>% 
  summarize(openhrs = sum(clemons),
            total = n(),
            percent = openhrs/total) %>% 
  ggplot(aes(x = hour, y = percent, color = day)) + 
  geom_line()


# Alderman standard hours ----
#   00:00 Sunday to 8:00 Monday (Open Sunday 10-midnight)
#   00:00 Monday to 8:00 Tuesday (Open Monday 8-midnight)
#   00:00 Tuesday to 8:00 Wednesday (Open Tuesday 8-midnight)
#   00:00 Wednesday to 8:00 Thursday (Open Wednesday 8-midnight)
#   00:00 Thursday to 8:00 Friday (Open Thursday 8-midnight)
#   21:00 Friday to 9:00 Saturday (Open Friday 8-9pm)
#   20:00 Saturday to 10:00 Sunday (Open Saturday 9-8pm)

open <- open %>% 
  mutate(alderman = 1) 

open <- open %>%
  mutate(alderman = if_else(day %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & hour < 8, 0, alderman),
         alderman = if_else(day == "Fri" & hour >= 21, 0, alderman),
         alderman = if_else(day == "Sat" & hour < 9, 0, alderman),
         alderman = if_else(day == "Sat" & hour >= 20, 0, alderman),
         alderman = if_else(day == "Sun" & hour < 10, 0, alderman))

# Alderman nonstandard hours ----
# Winter break/Intersession: Mon 1/1 closed; Tue 1/2-Thu 1/4 8-10, Fri 1/5 8-6, Sat 1/6 1-5, Sun 1/7 1-10; 
# Mon 1/8-Thu 1/11 8-10, Fri 1/12 8-6, Sat 1/13 1-5, Sun 1/14 closed; Mon 1/15 8-10, Tue 1/16 8-10
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-01-01"), 0, alderman),
         alderman = if_else(between(date, as.Date("2018-01-02"), as.Date("2018-01-04")) & hour < 8, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-01-02"), as.Date("2018-01-04")) & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-05") & hour < 8 | date == as.Date("2018-01-05") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-06") & hour < 13 | date == as.Date("2018-01-06") & hour > 17, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-07") & hour < 13 | date == as.Date("2018-01-07") & hour > 22, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-01-08"), as.Date("2018-01-11")) & hour < 8, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-01-08"), as.Date("2018-01-11")) & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-12") & hour < 8 | date == as.Date("2018-01-12") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-13") & hour < 13 | date == as.Date("2018-01-13") & hour > 17, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-14"), 0, alderman),
         alderman = if_else(date == as.Date("2018-01-15") & hour < 8 | date == as.Date("2018-01-15") & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-01-16") & hour < 8 | date == as.Date("2018-01-16") & hour > 22, 0, alderman))

# Spring break: Fri 3/2 close at 6, Sat 3/3-Sun 3/4 1-5; Mon 3/5-Thu 3/8 8-10; Fri 3/9 8-6; Sat 3/10 1-5; Sun 3/11 12-midnight
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-03-02") & hour < 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-03-03"), as.Date("2018-03-04")) & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-03-03"), as.Date("2018-03-04")) & hour > 17, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-03-05"), as.Date("2018-03-08")) & hour < 8, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-03-05"), as.Date("2018-03-08")) & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-03-09") & hour < 8 | date == as.Date("2018-03-09") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-03-10") & hour < 13 | date == as.Date("2018-03-10") & hour > 17, 0, alderman),
         alderman = if_else(date == as.Date("2018-03-11") & hour < 12, 0, alderman))

# Spring exams: Thu 5/3 - Thu 5/10 8-midnight, Fri 5/11 8-6, Sat 5/12 and Sun 5/13 1-5
open <- open %>% 
  mutate(alderman = if_else(between(date, as.Date("2018-05-03"), as.Date("2018-05-10")) & hour >= 8 & hour < 12, 1, alderman),
         alderman = if_else(date == as.Date("2018-05-11") & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-05-12"), as.Date("2018-05-13")) & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-05-12"), as.Date("2018-05-13")) & hour > 17, 0, alderman))

# Intersession: Mon 5/14-Thu 5/17 8-10, Fri 5/18 8-6, Sat 5/19 and Sun 5/20 8-5
open <- open %>% 
  mutate(alderman = if_else(between(date, as.Date("2018-05-14"), as.Date("2018-05-17")) & hour < 10, 0, alderman),
         alderman = if_else(date == as.Date("2018-05-18") & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-05-19"), as.Date("2018-05-20")) & hour > 17, 0, alderman))

# Summer hours: Mon 5/21-Thu 6/14; Mon 6/18-Thu 7/12; Mon 7/16-Wed 8/8
# Mon-Thu 8-10, Fri 8-6, Sat 1-5, Sun 1-10
open <- open %>% 
  mutate(alderman = if_else(between(date, as.Date("2018-05-21"), as.Date("2018-06-14")) & day %in% c("Mon", "Tue", "Wed", "Thu") & hour > 22, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-06-18"), as.Date("2018-07-12")) & day %in% c("Mon", "Tue", "Wed", "Thu") & hour > 22, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-07-16"), as.Date("2018-08-08")) & day %in% c("Mon", "Tue", "Wed", "Thu") & hour > 2, 0, alderman),         
         alderman = if_else(between(date, as.Date("2018-05-21"), as.Date("2018-06-14")) & day == "Fri" & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-06-18"), as.Date("2018-07-12")) & day == "Fri" & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-07-16"), as.Date("2018-08-08")) & day == "Fri" & hour > 18, 0, alderman),         
         alderman = if_else(between(date, as.Date("2018-05-21"), as.Date("2018-06-14")) & day == "Sat" & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-06-18"), as.Date("2018-07-12")) & day == "Sat" & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-07-16"), as.Date("2018-08-08")) & day == "Sat" & hour < 13, 0, alderman),         
         alderman = if_else(between(date, as.Date("2018-05-21"), as.Date("2018-06-14")) & day == "Sat" & hour > 17, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-06-18"), as.Date("2018-07-12")) & day == "Sat" & hour > 17, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-07-16"), as.Date("2018-08-08")) & day == "Sat" & hour > 17, 0, alderman),         
         alderman = if_else(between(date, as.Date("2018-05-21"), as.Date("2018-06-14")) & day == "Sun" & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-06-18"), as.Date("2018-07-12")) & day == "Sun" & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-07-16"), as.Date("2018-08-08")) & day == "Sun" & hour < 13, 0, alderman),         
         alderman = if_else(between(date, as.Date("2018-05-21"), as.Date("2018-06-14")) & day == "Sun" & hour > 22, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-06-18"), as.Date("2018-07-12")) & day == "Sun" & hour > 22, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-07-16"), as.Date("2018-08-08")) & day == "Sun" & hour > 22, 0, alderman),         
  )

# Summer exams: Fri 6/15-Sat 6/16; Fri 7/13-Sat 7/14; Thu 8/9-Fri 8/10
#   Fri 8-10, Sat 9-6
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-06-15") & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-06-16") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-07-13") & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-07-14") & hour > 18, 0, alderman))

# Intersession? Sat 8/11-Thu 8/23
#   Mon-Thu 8-10, Fri 8-6, Sat 1-5, Sun closed
open <- open %>% 
  mutate(alderman = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-23")) & day %in% c("Mon", "Tue", "Wed", "Thu") & hour > 22, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-23")) & day == "Fri" & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-23")) & day == "Sat" & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-23")) & day == "Sat" & hour > 17, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-08-11"), as.Date("2018-08-23")) & day == "Sun", 0, alderman))

# Move in: Sat 8/25 9-6, Sun 8/26 1-10
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-08-25") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-08-26") & hour > 22, 0, alderman),
         alderman = if_else(date == as.Date("2018-08-26") & hour < 13, 0, alderman))

# Summer holidays: 5/28, 7/4 closed
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-05-28"), 0, alderman),
         alderman = if_else(date == as.Date("2018-07-04"), 0, alderman))

# Fall Break: Fri 10/5 8-6; Sat, 10/6 1-5; Sun, 10/7 1-5, Mon, 10/8 8-10; Tue, 10/9 8-midnight
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-10-05") & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-10-06"), as.Date("2018-10-07")) & hour < 13, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-10-06"), as.Date("2018-10-07")) & hour > 17, 0, alderman),
         alderman = if_else(date == as.Date("2018-10-08") & hour > 22, 0, alderman))

# Thanksgiving: Tue, 11/20 8-6; Wed, 11/21 8-6; Thu, 11/22 closed; Fri, 11/23 1-5; Sat, 11/24 1-5; Sun, 11/25 12-midnight
open <- open %>% 
  mutate(alderman = if_else(date == as.Date("2018-11-20") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-11-21") & hour > 18, 0, alderman),
         alderman = if_else(date == as.Date("2018-11-22"), 0, alderman),
         alderman = if_else(date == as.Date("2018-11-23") & hour < 13 | date == as.Date("2018-11-23") & hour > 17, 0, alderman),
         alderman = if_else(date == as.Date("2018-11-24") & hour < 13 | date == as.Date("2018-11-24") & hour > 17, 0, alderman),
         alderman = if_else(date == as.Date("2018-11-25") & hour < 12, 0, alderman))

# Fall Exams: Sat 12/8 to Mon 12/17 Alderman is 8-midnight; Tue 12/18 8-6;  
# Holiday: Sat 12/22 - Mon 12/31-closed
open <- open %>% 
  mutate(alderman = if_else(between(date, as.Date("2018-12-08"), as.Date("2018-12-17")) & hour > 7, 1, alderman),
         alderman = if_else(date == as.Date("2018-12-18") & hour > 18, 0, alderman),
         alderman = if_else(between(date, as.Date("2018-12-22"), as.Date("2018-12-31")), 0, alderman))

# check
table(open$alderman)
open %>% group_by(day, hour) %>% 
  summarize(openhrs = sum(alderman),
            total = n(),
            percent = openhrs/total) %>% 
  ggplot(aes(x = hour, y = percent, color = day)) + 
  geom_line()


saveRDS(open, file = "clem_ald_hours.rds")

