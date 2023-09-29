#In this file we build the metaweb at European level

#1) Read all network and create a single dataset

#Load libraries
library(dplyr)

#1) Read all data in a unique dataset----
#Read all files with their paths
files <- dir("Data/Clean_data", pattern="*.rds", full.names=T)
files <- mixedsort(files)

#Generate list of lists
all_data = files %>%
map(readRDS) 

#Read all file names
file_names <- dir("Data/Clean_data", pattern="*.rds", full.names=F)
file_names <- mixedsort(file_names)

#Delete extension
file_names = str_replace(file_names, ".rds", "")
#Rename all elements of the list
names(all_data) <- file_names

#Select all cols from the list of lists----
data <- list()

for (i in 1:length(file_names)) {
  
  int_list <- all_data[[file_names[i]]]$InteractionData
  data[[i]] <- bind_rows(map(int_list, function(x) x %>% #map here is the same as a lapply
                dplyr::select(everything())),  .id = 'Network_id')
}

#Rename files from list  
names(data) <- file_names
#Prepare data to save it
all_list = data
all = bind_rows(data,  .id = 'Study_id')
#Check country levels
levels(factor(all$Country))
#Create another col with study id and network id together so we can count all individuals networks
master = all %>% 
mutate(Study_network_id = paste0(Study_id, Network_id))


