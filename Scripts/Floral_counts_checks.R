#Load library for merging datasets
library(dplyr)
#Read data
a = readRDS("Data/3_Final_data/Interaction_data.rds")
colnames(a)
b = readRDS("Data/3_Final_data//Flower_counts.rds")
colnames(b)
#See that both datasets has a column named "Flower_data_merger"

#Merge datasets
d = left_join(a, b, by = join_by(Study_id, Flower_data_merger),  suffix=c("",".y")) %>% select(-ends_with(".y"))



na_rows <- which(is.na(a$Flower_data_merger))
rows_with_na <- meta_count[na_rows, ]
print(rows_with_na)
