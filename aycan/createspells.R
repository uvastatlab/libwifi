## Trying for the wifi data 
library(anytime)
library(lubridate)
library(dplyr)
library(chron)
setwd("~/Box/WiFi_data")

dummy <- read.csv("dummy2.csv",stringsAsFactors = FALSE) %>% 
  dplyr::select(user,time)

dummy2 <- dummy 

dummy2$time <- mdy_hm(dummy2$time)

dummy2$Date <- as.Date(dummy2$time)
dummy2$Time <- format(as.POSIXct(dummy2$time, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H:%M:%S")

dummy2$Times <- chron(times=dummy2$Time)


spell <- dummy2 %>% 
  dplyr::select(Date,user,Times) %>% 
  arrange(Date,user,Times)

# Compare time differences between time column with the previous row after arranging them
# If the time difference is bigger than 30, then treat that group as a different spell 

# This is not an ideal way to get the difference but it works 

spell <- spell %>% 
  group_by(Date,user) %>% 
  mutate(diff = Times - lag(Times)) 

# Creating a groupid column I can group by different spells with 

spell2 <- spell %>% 
  filter(is.na(diff)|diff>0.0208334)

spell2 <- spell2 %>% 
  group_by(Date,user) %>% 
  mutate(groupid = row_number())

spellnew <- left_join(spell,spell2)

spellnew <- spellnew %>% 
  group_by(Date,user) %>% 
  fill(groupid)

spellcount <- spellnew %>% 
  count(Date,user,groupid) %>% 
  rename(mins = n) %>% 
  mutate(mins = mins*30)

write.csv(spellcount, "uniquedurations.csv",quote=FALSE,row.names=FALSE)

       
        