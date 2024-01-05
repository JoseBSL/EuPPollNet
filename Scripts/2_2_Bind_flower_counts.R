#------------------------------------------------#
#1) Read all networks and merge flower count data
#------------------------------------------------#
#Load libraries
library(dplyr) 
library(stringr)
library(gtools)
library(purrr)

#--------------------------#
#Read all files-----
#--------------------------#
files <- dir("Data/2_Processed_data", pattern="*.rds", full.names=T)
files <- mixedsort(files)

#Generate list of lists
all_data = files %>%
map(readRDS) 

#Read all file names
file_names <- dir("Data/2_Processed_data", pattern="*.rds", full.names=F)
file_names <- mixedsort(file_names)

#Delete extension
file_names = str_replace(file_names, ".rds", "")
#Rename all elements of the list
names(all_data) <- file_names

str(all_data$`1_Bartomeus`$FlowerCount[[1]])
str(all_data$`2_Petanidou`$FlowerCount[[1]])




#--------------------------#
#Merge-----
#--------------------------#
#Select all cols from the list of lists
data <- list()
for (i in 1:length(file_names)) {

  int_list <- all_data[[file_names[i]]]$FlowerCount
  data[[i]] <- bind_rows(map(int_list, function(x) x %>% #map here is the same as a lapply
                dplyr::select(everything())),  .id = 'Network_id')
}


#Rename files from list  
names(data) <- file_names
#Prepare data to save it
all_list = data
all = bind_rows(data,  .id = 'Study_id')
#Create another col with study id and network id together so we can count all individuals networks
flower_counts = all %>% 
mutate(Study_network_id = paste0(Study_id, Network_id))

#Save floral counts (as final file for now)
#Needs some checking....
saveRDS(flower_counts, "Data/3_Final_data/Flower_counts.rds")
