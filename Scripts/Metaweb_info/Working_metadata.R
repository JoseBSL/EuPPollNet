

#Load library
library(googlesheets4)
library(lubridate)
library(dplyr)
#Generate list of studies with individual networks 
data = readRDS("Data/Interactions_uncounted.rds")

#Select cols of interest
dates = data %>% 
select(Study_id, Network_id,Sampling_method, Day, Month, Year) %>% 
mutate(Date = dmy(paste(Day, Month, Year, sep = "-")))
#Obtain date information
dates1 = dates %>% 
group_by(Study_id, Network_id, Sampling_method) %>% 
summarise(Min_date = min(Date), 
          Max_date= max(Date), 
          Sampling_days = n_distinct(Date)) %>% 
mutate(Sampling_period = as.numeric(Max_date - Min_date)) %>% 
mutate(Study_number = as.integer(str_extract(Study_id, "[^_]+")), 
       .before=Study_id) %>% 
arrange(Study_number) %>% 
mutate(Sampling_period = case_when(!is.na(Sampling_period) ~ Sampling_period + 1,
TRUE ~ NA)) %>% 
mutate(Sampling_days = case_when(!is.na(Sampling_period) ~ Sampling_days,
TRUE ~ NA))

(ss <- gs4_create("Sampling_details", sheets = dates1))



