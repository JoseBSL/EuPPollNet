#Explore habitat data (as classified in SafeNet)

#Load libraries
library(forcats)
library(ggplot2)
library(dplyr)

#Read data
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")
#Check cols
colnames(data)
#Extract cols of interest (for now)
habitat_info = data %>% 
select(Study_id, Network_id,Latitude, Longitude, SafeNet_habitat) %>% 
distinct()

#Explore graphically
ggplot(habitat_info, aes(fct_infreq(SafeNet_habitat))) +
geom_bar(fill = "gray20") +
theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
ylab("Number of sites") +
xlab(NULL)

habitat_info_study = data %>% 
select(Study_id, SafeNet_habitat) %>% 
distinct()

#Explore graphically
ggplot(habitat_info_study, aes(fct_infreq(SafeNet_habitat))) +
geom_bar(fill = "gray20") +
theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
ylab("Number of studies") +
xlab(NULL)
