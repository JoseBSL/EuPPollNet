#Read all files with their paths
files <- dir("Data/Clean_data", pattern="*.RData", full.names=T)

#Generate list of lists
all_data = files %>%
map(readRDS) 

#Read all file names
file_names <- dir("Data/Clean_data", pattern="*.RData", full.names=F)
#Delete extension
file_names = str_replace(file_names, ".RData", "")
#Rename all elements of the list
names(all_data) <- file_names
