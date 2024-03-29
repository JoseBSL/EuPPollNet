#Load library for merging datasets
library(dplyr)
#Read data
a = readRDS("Data/3_Final_data/Interaction_data.rds")
colnames(a)
b = readRDS("Data/3_Final_data//Flower_counts.rds")
colnames(b)
#See that both datasets has a column named "Flower_data_merger"

a1 = a %>% filter(Study_id == "38_Maurer")
b1 = b %>% filter(Study_id == "38_Maurer")

#Merge datasets
d = left_join(a1, b1, by = join_by(Study_id, Flower_data_merger),  suffix=c("",".y")) %>% select(-ends_with(".y"))




check = d %>% 
filter(Study_id == "26_Scheper") %>% 
filter(is.na(Flower_data_merger))


