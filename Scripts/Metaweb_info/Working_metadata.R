

#Generate list of studies with individual networks 
data = readRDS("Data/Interactions_uncounted.rds")

#Select cols of interest
metadata = data %>% 
select(Study_id, Network_id, Sampling_method) %>% 
distinct() %>% 
mutate(Sampling_time = "") %>% 
mutate(Sampling_area = "") 

#Load library
#library(googlesheets4)
#Load it to google sheets
#(ss <- gs4_create("Sampling_details", sheets = metadata))
