library(tidyverse)

#Read comma delimited
data <- read_csv("Data/Raw_data/Michez/Michez.csv")

#Check colnames and first 5 rows
colnames(data)
data %>% slice_head(n = 5)

#Things to check
#Add sampling area??
#Rename species Bombus cf. terrestris







