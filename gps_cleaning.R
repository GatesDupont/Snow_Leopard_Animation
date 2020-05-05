library(readxl)
library(dplyr)

raw_data <- read.csv("data/ali_sl_gps_fix.csv") %>%
  mutate(Latitude = ï..Latitude)
