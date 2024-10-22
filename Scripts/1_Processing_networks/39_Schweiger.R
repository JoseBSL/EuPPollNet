#DATASET NUMBER 39; Schweiger

#Data sent by Oliver Schweiger. This dataset belongs to the VOODOO project
#https://voodoo-project.eu/about

#Read empty templates to compare with
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
data <- read_csv("Data/1_Raw_data/39_Schweiger/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)

#Because we only consider pollinators that visited flowers
#We filter out the ones that do not have listed a plant species  
data = data %>% 
filter(Plant_species != "no flower") %>% 
filter(Plant_species != "missing")

#Add flower info cols
data = data %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Flower_data = "Yes") 

#Exclude 5 rows with missing coordinates for now
data = data %>% 
filter(!is.na(Latitude))

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Column to merge with interaction data
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_",
word(Plant_species, 2), Site_id, "_", Day, "_", Month, "_", Year))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- 
FlowerCount = read_csv("Data/1_Raw_data/39_Schweiger/Flower_count.csv")

#Comments
FlowerCount = FlowerCount %>% 
mutate(Comments = NA) %>% 
mutate(Flower_data_merger = NA)

#Column to merge with interaction data
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_",
word(Plant_species, 2), Site_id, "_", Day, "_", Month, "_", Year))

#Drop not needed vars
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = "Unpublished",
  Dataset_description = "This dataset was generated within the VOODOO project
  and documents 16 different landscapes in agricultural and urban areas of Central Germany.
  We followed a landscape-level approach, where a site is defined by a 1km radius.
  Within each site we placed transects on a stratified random basis in each of present habitat types.
  The total length of the transect was 1km per site, but split according to the relative proportions of the different habitat types.
  If multiple patches of a particular habitat type were present,
  transects can occur in different patches per habitat typ.
  Three sampling rounds were undertaken to cover the full activity period of pollinators.
  During each sampling round, flower densities were assessed per species with multiple plots of 1 sqm.
  The number of plots depended on the length of the respective transect.
  Flower densities were calculated as average across all pllots per transect.
  Except for Asteraceae, single flowers were counted.
  In case you need flower units, we can provide this.
  The nested sampling structure is reflected in the Site code:
  Site_habitat type_transect ID. 
  With this you can aggregate per landscape or per habitat type per landscape.
  Note: the location of the transect was adjusted to the management and flower availability.
  Thus, the IDs for habitat and transect are in some cases not constant across the three sampling rounds.
  As a second identifyer, particularly for linking network and flower data, you need the sampling round.
  This is coded in the comments column (rounds 1,2,3) as I did not want to mess up the structure of the template.",
  Taxa_recorded = "Bees and syrphids",
  Sampling_year = "2021",
  Country = "Germany",
  Habitat = "Agricultural and urban areas",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "3",
  Sampling_method = "Transects",
  Sampling_area_details = "1km length*width?",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = NA,
  Sampling_time_details = NA, 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = NA, #time *sites * rounds
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Oliver Schweiger", "Jonna M. Heuschele", "Yicong Liu", "Christophe Dominik"),
  Orcid = c("0000-0001-8779-2335", "0009-0005-3242-9412", "0009-0003-2573-8132", "0000-0001-6310-1632"),
  E_mail = c("oliver.schweiger@ufz.de", "jonna.heuschele@ufz.de", "liu.yicong@ufz.de", "christophe.dominik@ufz.de"))

#Save data ----
#Create metadata list
Schweiger <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Schweiger) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Schweiger, file="Data/2_Processed_data/39_Schweiger.rds")


