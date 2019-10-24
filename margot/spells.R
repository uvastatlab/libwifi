library(tidyverse)
library(lubridate)

clem_spells <- read_csv('clemons-spells.csv')
ald_spells <- read_csv('alderman-spells.csv')
har_spells <- read_csv('harrison-spells.csv')
clem_spells <- clem_spells %>% 
  mutate(minstay = (daystay*24)+hour(hourstay)+(minute(hourstay)/60),
         maxstay = minstay+0.5,
         location = "clemons")
ald_spells <- ald_spells %>% 
  mutate(minstay = (daystay*24)+hour(hourstay)+(minute(hourstay)/60),
         maxstay = minstay+0.5,
         location = "alderman")
har_spells <- har_spells %>% 
  mutate(minstay = (daystay*24)+hour(hourstay)+(minute(hourstay)/60),
         maxstay = minstay+0.5,
         location = "harrison")
spells <- bind_rows(clem_spells, ald_spells, har_spells)
spells <- spells %>% mutate(date = as.Date(time))

spells %>% filter(maxstay < 10) %>% ggplot() +
  geom_histogram(aes(maxstay, fill = location),binwidth = 0.5) +
  facet_grid(cols = vars(location), scales = "free")

spells %>% 
  group_by(location,user,yearday) %>% 
  summarize(count = n()) %>%
  ggplot() +
  geom_histogram(aes(count, fill = location),binwidth = 1) +
  facet_grid(cols = vars(location), scales = "free")

spells %>% filter(location != "harrison") %>% group_by(location, date) %>% 
  summarize(meanstay = mean(maxstay),count = n()) %>%
  ggplot() +
  geom_line(aes(x = date, y = meanstay, color = location)) +
  scale_y_continuous(limits = c(0,24))
