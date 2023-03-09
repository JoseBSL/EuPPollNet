library('gtools') #Library that allows to sort files by numerical order
library(tidyverse)

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


#Select coordinates from the list of lists----
data <- list()

for (i in 1:length(file_names)) {
  
  int_list <- all_data[[file_names[i]]]$Authorship
  data[[i]] <- bind_rows(int_list)
}

#Rename list and 
names(data) <- file_names
data = as_tibble(bind_rows(data,  .id = 'Study_id'))

#Save metadata file
write_csv(data, "Data/Processing/Authorship.csv")
write_csv(data, "~/R_projects/SafeNet/Data/Authorship.csv")
write_csv(data, "~/R_Projects/SafeNetWeb/Data/Authorship.csv")

