library('gtools') #Library that allows to sort files by numerical order

#Read all files with their paths
files <- dir("Data/Clean_data", pattern="*.RData", full.names=T)
files <- mixedsort(files)

#Generate list of lists
all_data = files %>%
map(readRDS) 

#Read all file names
file_names <- dir("Data/Clean_data", pattern="*.RData", full.names=F)
file_names <- mixedsort(file_names)

#Delete extension
file_names = str_replace(file_names, ".RData", "")
#Rename all elements of the list
names(all_data) <- file_names




#select unique cases from the list of lists
data <- list()

for (i in 1:length(file_names)) {
  
  int_list <- all_data[[file_names[i]]]$InteractionData
  data[[i]] <- bind_rows(lapply(int_list, function(x) x %>% 
                select(Longitude, Latitude)),  .id = 'id')
}

#Rename list and 
names(data) <- file_names
data = bind_rows(data)
