library('gtools') #Library that allows to sort files by numerical order
library(tidyverse)

#Read all files with their paths
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
write_csv(data, "Data/Manuscript_info/Authorship.csv")


#Prepare author data for article
authors = data
#Order dataset by last word of author's name
ordered_data = authors[order(word(authors$Coauthor_name,-1)),]
#Filter for duplicated author names
ordered_authors = ordered_data %>% 
  distinct(Coauthor_name, .keep_all = TRUE)
#Add Will Glenny and Nerea Montes Perez
#Nacho last? Tiffany 2nd?
#NOTE: GITA BENADI did not accept the invitation of being a co-author- This is the NA. Fix it and exclude it here 
ordered_authors %>% 
summarise(col = paste(Coauthor_name, collapse=", ")) %>% 
pull()


