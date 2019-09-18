# wifi data
# some exploration

library(tidyverse)
wifi <- readRDS("/Users/jcf2d/Box Sync/WiFi_data/wifi.rds")
wifi$location <- factor(wifi$location)
head(wifi)

# number of records with Department, Display_Department, MBU, Registrar_School
# and uvaPersonIAMAffiliation all missing
vars <- names(wifi)[-(1:7)]
wifi <- mutate(wifi, allNA = apply(wifi[vars], 1, function(x)all(is.na(x))))

scales::percent(mean(wifi$allNA))
scales::comma(sum(wifi$allNA))

# all NA by location
wifi %>% 
  group_by(location) %>% 
  summarise(sum_na = sum(allNA),
            mean_na = mean(allNA))


wifi_allNA <- wifi %>% filter(allNA)
wifi_noNA <- wifi %>% filter(!allNA)

nrow(wifi_noNA) + nrow(wifi_allNA) == nrow(wifi)


# investigating mac in both wifi and wifi_allNA
length(intersect(wifi_noNA$mac, wifi_allNA$mac))
which_mac <- intersect(wifi_noNA$mac, wifi_allNA$mac)

wifi2 <- filter(wifi, mac %in% which_mac)
map(wifi2[vars], ~ table(.x))


wifi2 %>% 
  group_by(location) %>% 
  summarise(sum_na = sum(allNA),
            mean_na = mean(allNA))

# It appears that records having Department, Display_Department, MBU,
# Registrar_School and uvaPersonIAMAffiliation all missing are not always that
# way. For example, see mac 3673 (user 12800). Has only one instance of allNA at
# Clemons, but has all NA at Alderman.

wifi2 %>% 
  filter(mac == 3673) %>% 
  group_by(location) %>% 
  summarise(sum_na = sum(allNA),
            mean_na = mean(allNA))

wifi2 %>% 
  group_by(mac, location) %>% 
  summarise(mean_na = round(mean(allNA), 2)) %>% 
  as.data.frame()

# There seems to be no pattern. Sometimes all missing at Alderman, other times
# all missing at Clemons.


# mac in wifi_noNA not in wifi_allNA
length(setdiff(wifi_noNA$mac, wifi_allNA$mac))

# mac in wifi_allNA not in wifi
length(setdiff(wifi_allNA$mac, wifi_noNA$mac))

# Appears to be over 16,500 unique macs that always have all `vars` missing

n_distinct(wifi$mac)
length(setdiff(wifi_allNA$mac, wifi_noNA$mac))/ n_distinct(wifi$mac)

# That's about 26% of the unique macs

save(list = ls(), file = "workspace.Rda")
