# Library Wifi Project
# Comboine monthly files from three locations
# Create anonymous user/mac ids from hashed ids
# Generate wifi.rds for use in analysis
# Clay Ford

# Libraries
library(readxl)
library(purrr)
library(dplyr)
library(lubridate)

# Data
harrison <- list.files("../wifidata/harrison/harrison/")
alderman <- list.files("../wifidata/alderman/alderman/")
clemons <- list.files("../wifidata/clemons/clemons/")

h.list <- map(harrison, ~ read_excel(paste0("/Users/jcf2d/Desktop/wifidata/harrison/harrison/",.x)))
a.list <- map(alderman, ~ read_excel(paste0("/Users/jcf2d/Desktop/wifidata/alderman/alderman/",.x)))
c.list <- map(clemons, ~ read_excel(paste0("/Users/jcf2d/Desktop/wifidata/clemons/clemons/",.x)))

# Bind data
h.df <- bind_rows(h.list)
h.df$location <- "harrison"
a.df <- bind_rows(a.list)
a.df$location <- "alderman"
c.df <- bind_rows(c.list)
c.df$location <- "clemons"
wifi <- bind_rows(h.df, a.df, c.df)

rm(list = grep(pattern = "^w", x = ls(), invert = TRUE, value = TRUE))


# Generate anonymous id numbers
wifi$mac <- unclass(factor(wifi$enc_mac))
wifi$user <- unclass(factor(wifi$enc_user))

# drop factor levels preserved as attribute
attr(wifi$mac, which = "levels") <- NULL
attr(wifi$user, which = "levels") <- NULL

stopifnot(length(unique(wifi$enc_mac)) == length(unique(wifi$mac)))
stopifnot(length(unique(wifi$enc_user)) == length(unique(wifi$user)))

# missing user?
sum(is.na(wifi$user)) # 9034
sum(is.na(wifi$mac)) # 0

# impute missing user ids using MPC's suggestion
wifi <- wifi %>% mutate(user = if_else(!is.na(user), user, 40000L + mac))
                        
# save with hash
# saveRDS(wifi, file = "wifi_hash.rds")
write.csv(wifi, file = "/Users/jcf2d/Desktop/wifidata/wifi_all_libraries.csv", row.names = FALSE)

# drop hash
wifi$enc_mac <- NULL
wifi$enc_user <- NULL

# add extra date columns
names(wifi)[1] <- "time"
wifi$time <- ymd_hms(wifi$time, tz = "US/Eastern")
wifi$day <- wday(wifi$time, label = TRUE)
wifi$month <- month(wifi$time, label = TRUE)

# reorder columns
wifi <- select(wifi, location, user, mac, time, month, day, everything())

# sort by location and time
wifi <- arrange(wifi, location, time)

# save without hash
saveRDS(wifi, file = "wifi.rds")


