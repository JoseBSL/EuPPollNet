#DATASET NUMBER 2; Petanidou & Nakas

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/2_Petanidou/Interaction_data.csv")

#Quick clean of the interaction data
data <- data %>% 
mutate(Plant_species = word(Plant_species, 1, 2), 
Pollinator_species = word(Pollinator_species, 1, 2)) %>%
rename(Latitude = latitude)  %>% 
rename(Longitude = longitude)  %>% 
mutate(Coordinate_precision = "10m")  

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
flower_count <- read_csv("Data/Raw_data/2_Petanidou/Flower_count.csv")

#Add leading 0's to days and month under 10
flower_count$Month <- ifelse(as.numeric(flower_count$Month) < 10, paste0("0", flower_count$Month), flower_count$Month)
flower_count$Day <- ifelse(as.numeric(flower_count$Day) < 10, paste0("0", flower_count$Day), flower_count$Day)

Flower_count <- flower_count %>%
select(Day, Month, Year, Site_id, Plant_species, Flower_count) %>%
mutate(Units = gsub("#", "Number", flower_count$Units))
 

#Prepare metadata data ----
meta <- tibble(
Doi = NA,
Dataset_description = "This dataset documents 6 different sites in Chios Island
Greece. The purpose of the samplings was to study post-fire succession
of plant-pollinator system so we have included data from two sites that
they were burnt three years before sampling. The remaining four sites
are unburnt scrublands with two of them being in the margins of cultivations
(olive grove and mastic trees). Each year 3 visits to each site
were performed and in each visit we measured both plant-pollinator
interactions and flower abundance. All samplings were performed during
the period that most pollinators are active and most plants are flowering
(April – End of June). We have included data of one year of sampling
but two more years are available.",
Taxa_recorded = "All flower visitors belonging to the orders:
Hymenoptera (with the exception of Formicidae), Diptera
(we mainly focused in Syrphidae and Bombylidae), Coleoptera and Lepidoptera.")

#Prepare authorship data ----
authors <- tibble(
  Coauthor_name = c("Theodora Petanidou", "Georgios Nakas"),
  Orcid = c("0000-0003-1883-0945", "0000-0003-3023-5831"),
  E_mail = c("tpet@aegean.gr", "nakas.g@geo.aegean.gr"))


#Save data ----
#Create metadata list
Metadata <- list(meta) 
Authorship <- list(authors) 
FlowerCount <- list(flower_count) 
#Create list with all dataframes of interest
Petanidou <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Petanidou) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Petanidou, file="Data/Clean_data/2_Petanidou.RData")

