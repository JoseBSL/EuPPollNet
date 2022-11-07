#DATASET NUMBER 26; Albrecht (year 2014)

#Read templates to compare with
source("Scripts/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(stringr)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/26_27_Albrecht/Interaction_data1.csv")

#FIRST fix pollinator species (substitute dot by space)
data = data %>% 
mutate(Pollinator_species = str_replace_all(Pollinator_species, "\\.", " "))
#Second fix plant species (This seems more tricky) 
#I have to read the flower count data and see if they match after some preparation
Plant_species <- read_csv("Data/Raw_data/26_27_Albrecht/Flower_count1.csv") %>% select(Plant_species)
#Now create a column with the 3 first letters of genus and specific epithet separated by a point :)
Plant_species = Plant_species %>%
mutate(Complicated_name = str_replace_all(Plant_species, "\\.", " ")) %>% 
mutate(Complicated_name1 = word(Complicated_name, 1)) %>% 
mutate(Complicated_name2 = word(Complicated_name, 2)) %>% 
mutate(Complicated_name1 = substr(Complicated_name1 , start = 1 , stop = 3 )) %>% 
mutate(Complicated_name2 = substr(Complicated_name2 , start = 1 , stop = 3 )) %>% 
mutate(Complicated_name = paste0(Complicated_name1, "." ,Complicated_name2)) %>% 
distinct(Plant_species, Complicated_name) %>% #Now data is ready
mutate(Complicated_name = trimws(Complicated_name))

#Make first letter to capital in the original interaction data
data = data %>% 
mutate(Plant_species = str_to_title(Plant_species)) %>% 
rename(Complicated_name = Plant_species) %>%#make sure there are not leading or trailing spaces
mutate(Complicated_name = trimws(Complicated_name))

#Now make a left join of the curated species name with the original dataset
data = left_join(data, Plant_species)

setdiff(data$Complicated_name, Plant_species$Complicated_name)
