#DATASET NUMBER 46; Knight-Poland (nature reserve, 1 day)
#Dataset sent by Tiffany

source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/46_Knight/Interaction_data.csv", locale = locale(encoding = "latin1"))

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
flower_count <- read_csv("Data/Raw_data/46_Knight/Flower_count.csv")

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = "unpublished",
Dataset_description = "The study was conducted in the Nature
Reserve Góra Gipsowa. We established 10 transects, each
measuring 30x2m. Transects were placed with a minimum
distance of 30m between them. We used the standardized
transect walks to also quantify pollinator species and
plant-pollinator interactions. One collector walked each
transect for an active sampling period of 15 min. All
Hymenoptera, Diptera and Lepidoptera that contacted the
reproductive structures of the flowers were treated as
potential pollinators and collected using sweeping nets",
Taxa_recorded = "Hymenoptera, Diptera and Lepidoptera",
Sampling_year = "2018",
Country = "Poland and Czech Republic",
Habitat = "Grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "1",
Sampling_method = "Transects",
Sampling_area_details = "30 by 2 meter transects (10)",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA ,
Sampling_time_details = NA,
Sampling_time_species_round_min = "15 min per transect",
Sampling_time_total_min = NA, 
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Tiffany M. Knight", "Demetra Rakosy",
                    "Elena Motivans Švara", "Valentin Ştefan",
                    "Reinart Feldmann", "Arkadiusz Nowak",
                    "Sebastian Świerszcz", "Elisabeth Kühn"),
  Orcid = c("0000-0003-0318-1567", "0000-0001-8010-4990",
            "0000-0002-2407-9564", "0000-0002-4757-8008",
            "", "0000-0001-8638-0208", "0000-0003-2035-0035",
            "0000-0001-8894-2462"),
  E_mail = c("tiffany.knight@idiv.de", "demetra.rakosy@gmail.com",
             "elena.motivans@ufz.de", "valentin.stefan@idiv.de",
             "reinart.feldmann@ufz.de", "anowak@uni.opole.pl", 
             "seb.swierszcz@gmail.com", "elisabeth.kuehn@ufz.de"))

#Save data ----
#Create metadata list
Knight <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Knight) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Knight, file="Data/Clean_data/46_Knight.rds")




