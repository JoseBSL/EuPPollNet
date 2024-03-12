#------------------------------------------------#
#Prepare metadata at NETWORK LEVEL from Safenet
#------------------------------------------------#

#Workflow:
#1)Load libraries and data
#2)Prepare date information
#Info about starting date, finishind date,
#sampling period and days sampled
#3)Prepare taxonomic information
#Info from main orders (% and number of them)
#Info from other order (number of them)
#Total interactions

#--------------------------------------#
#1)Load libraries and data-----
#--------------------------------------#
#Load libraries
library(googlesheets4) #To upload to google sheets
library(lubridate) #To operate with dates
library(dplyr) #Handling data
library(tidyr) #Reshape data (wide format)
library(stringr)
#Load data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))

#--------------------------------------#
#2)Prepare date information-----
#--------------------------------------#
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

#--------------------------------------#
#3)Prepare taxonomic information-----
#--------------------------------------#

#Select cols of interest and prepare data for processing
taxo_info = data %>% 
select(Study_id, Network_id, 
       Pollinator_accepted_name, Pollinator_order) %>% 
group_by(Study_id,Network_id, Pollinator_order) %>% 
count(Pollinator_order) 

#Select any order that is not (Cole, Lepi,Hymeno, Dip)
main_orders = c("Hymenoptera", "Lepidoptera", "Diptera", "Coleoptera")
#Extract levels in a vector for sum non-main orders within pipes
non_main_orders = taxo_info %>% 
ungroup() %>% 
select(Pollinator_order) %>% 
distinct() %>% 
filter(!Pollinator_order %in% main_orders) %>% 
pull()
#Extract levels in a vector from all orders
all_orders = taxo_info %>% 
ungroup() %>% 
select(Pollinator_order) %>% 
distinct() %>% 
pull()

#Convert to wide format with cols of interest
#Steps:
#1st conert to wide
#2nd create a column with runing number to arrange studies
#3rd sum number of non-main orders
#4th sum number of main orders
#5th sum number of interactions across orders
#6th select cols of interest
#7th convert to percentage main order cols
taxo_info1 = taxo_info %>% 
pivot_wider(names_from = Pollinator_order,
            values_from = n) %>% #1st
mutate(Study_number = as.integer(str_extract(Study_id, "[^_]+")), 
       .before=Study_id) %>% #2nd
arrange(Study_number) %>% #2nd
mutate(Other_orders = length(non_main_orders) - #3rd
         sum(is.na(c_across(non_main_orders)), #3rd
         na.rm=TRUE)) %>% #3rd
mutate(Number_of_main_orders_recorded = (4 - #4th
         sum(is.na(Hymenoptera) + is.na(Diptera) + #4th
             is.na(Lepidoptera) + is.na(Coleoptera)))) %>% #4th
mutate(Total_interactions = sum(across(all_orders),  na.rm = TRUE)) %>% #5th
select(Study_number, Study_id, Network_id, Hymenoptera, Diptera,#6th
       Lepidoptera, Coleoptera, Number_of_main_orders_recorded, #6th
       Other_orders, Total_interactions) %>%  #6th
mutate(across(main_orders, ~ formatC(.x / Total_interactions,digits = 2, format = "f"))) %>% #7th
rename(Hymenoptera_percent = Hymenoptera,
       Diptera_percent = Diptera,
       Lepidoptera_percent = Lepidoptera,
       Coleoptera_percent = Coleoptera)


#Metadata study/network level
metadata = left_join(dates1, taxo_info1)

#Load data to google sheets
(ss <- gs4_create("Network_level", sheets = metadata))



