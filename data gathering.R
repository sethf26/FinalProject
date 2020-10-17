library(dplyr)
library(tidyverse)
library(readxl)
library(plyr)

NFL_2016_label <- read_excel("raw_data/NFL_2016_label.xlsx", 
                             skip = 3)
NFL_2016 <- NFL_2016_label%>%
  mutate(year = 2016)

NFL_2017_label <- read_excel("raw_data/NFL_2017_label.xlsx", 
                             skip = 3)
NFL_2017 <- NFL_2017_label%>%
  mutate(year = 2017)

NFL_2018_label <- read_excel("raw_data/NFL_2018_label.xlsx", 
                             skip = 3)
NFL_2018 <- NFL_2018_label%>%
  mutate(year = 2018)

NFL_Full_stat <- join(NFL_2016,NFL_2017,by = "year", type = "full")
NFL_Full_stat <- join(NFL_Full_stat,NFL_2018,by = "year", type = "full")

Combine_2016 <- read_excel("raw_data/Combine_2016.xlsx")
Combine_2016 <- Combine_2016%>%
  mutate(year = 2016)

Combine_2017 <- read_excel("raw_data/Combine_2017.xlsx")
Combine_2017 <- Combine_2017%>%
  mutate(year = 2017)

Combine_2018 <- read_excel("raw_data/Combine_2018.xlsx")
Combine_2018 <- Combine_2018%>%
  mutate(year = 2018)

Combine_Full_stat <- join(Combine_2016,Combine_2017,by = "year", type = "full")
Combine_Full_stat <- join(Combine_Full_stat,Combine_2018,by = "year", type = "full")