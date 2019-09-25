# combine files

library(readxl)
library(purrr)
library(dplyr)
library(lubridate)
harrison <- list.files("harrison/harrison/")
alderman <- list.files("alderman/alderman/")
clemons <- list.files("clemons/clemons/")

h.list <- map(harrison, ~ read_excel(paste0("harrison/harrison/",.x)))
a.list <- map(alderman, ~ read_excel(paste0("alderman/alderman/",.x)))
c.list <- map(clemons, ~ read_excel(paste0("clemons/clemons/",.x)))

h.df <- bind_rows(h.list)
h.df$location <- "harrison"
a.df <- bind_rows(a.list)
a.df$location <- "alderman"
c.df <- bind_rows(c.list)
c.df$location <- "clemons"
wifi <- bind_rows(h.df, a.df, c.df)

rm(list = grep(pattern = "^w", x = ls(), invert = TRUE, value = TRUE))

# generate anonymous id numbers
wifi$mac <- unclass(factor(wifi$enc_mac))
wifi$user <- unclass(factor(wifi$enc_user))

stopifnot(length(unique(wifi$enc_mac)) == length(unique(wifi$mac)))
stopifnot(length(unique(wifi$enc_user)) == length(unique(wifi$user)))

# missing enc_user?
sum(is.na(wifi$enc_user)) # 9034
sum(is.na(wifi$enc_mac)) # 0


# save with hash
saveRDS(wifi, file = "wifi_hash.rds")

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


