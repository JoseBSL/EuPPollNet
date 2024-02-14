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



library(readr)
library(tibble)
#Read data
data = read_csv("Data/Manuscript_info/Authorship.csv")
#Include Nerea Montes and Will Glenny
colnames(data)
#Add rows
data = data %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Nerea Montes-Perez", 
        Orcid = "0000-0002-5212-4537", 
        E_mail = "montespereznerea@gmail.com")
#Add rows
data = data %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Will Glenny", 
        Orcid = "0000-0002-2522-3775", 
        E_mail = "willglenny@gmail.com")

#Exclude Gita Benadi. Gita didn't want to be included as is longer active in science
data = data %>% 
filter(!Coauthor_name == "Gita Benadi")

#Exlude Tiffany and Nacho to place them in the right order
second_info = data %>% 
filter(Coauthor_name == "Tiffany M. Knight") %>% 
  distinct(Coauthor_name, .keep_all = TRUE)
last_info = data %>% 
filter(Coauthor_name == "Ignasi Bartomeus") %>% 
  distinct(Coauthor_name, .keep_all = TRUE)

data = data %>% 
filter(!Coauthor_name == "Tiffany M. Knight" & !Coauthor_name =="Ignasi Bartomeus")


#Prepare author data for article
authors = data
#Order dataset by last word of author's name
ordered_data = authors[order(word(authors$Coauthor_name,-1)),]
#Filter for duplicated author names
ordered_authors = ordered_data %>% 
  distinct(Coauthor_name, .keep_all = TRUE)

#Add Nacho to last
ordered_authors = bind_rows(ordered_authors, last_info)
#Add Tiffany second 
ordered_authors = bind_rows(second_info, ordered_authors)
#Add myself
ordered_authors = ordered_authors %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Jose B. Lanuza", 
        Orcid = "0000-0002-0287-409X", 
        E_mail = "barragansljose@gmail.com", .before=1)

ordered_authors_unique = ordered_authors %>% 
distinct(Coauthor_name)

#Print authors
ordered_authors %>% 
summarise(col = paste(Coauthor_name, collapse=", ")) %>% 
pull()

#Save 
write_csv(ordered_authors, "Data/Manuscript_info/Authorship_ordered.csv")

