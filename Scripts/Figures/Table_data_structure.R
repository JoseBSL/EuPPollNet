
#Prepare table with data structure
data = readRDS("Data/3_Final_data/safenet_database.rds")

#Create variables
vars = colnames(data)
#Create table
d = tibble(Variable = vars)

d = d %>% 
mutate(Variable = str_replace(Variable, "Latitude", "Latitude-Longitude")) %>% 
filter(!Variable == "Longitude")

#Add descriptor
my_tibble <- d %>%
  mutate(Description = case_when(
    Variable == "Study_id" ~ "Identifier of the study",
    Variable == "Network_id" ~ "Identifier of a site sampled within a study",
    Variable == "Sampling_method" ~ "Type of plant-pollinator sampling",
    Variable == "Authors_habitat" ~ "Type of habitat as described by the authors",
    Variable == "SafeNet_habitat" ~ "Type of habitat homogenized across studies",
    Variable == "Bioregion" ~ "European biogiographical regions",
    Variable == "Country" ~ "Country where the plant-pollinator interaction was observed",
    Variable == "Locality" ~ "Locality where the plant-pollinator interaction was observed",
    Variable == "Latitude-Longitude" ~ "Coordinates of the observed interaction in decimal degrees",
    Variable == "Date" ~ "Year, month and day when the observation took place",
    Variable == "Interaction" ~ "Number of interactions. By default is 1 as interactions are provided ungrouped",
    Variable == "Plant_original_name" ~ "Plant species name given by the authors",
    Variable == "Plant_accepted_name" ~ "Harmonized plant species name in the database",
    Variable == "Plant_rank" ~ "Taxonomic rank of the observation",
    Variable == "Plant_order" ~ "Order taxonomic rank of the observed plant species",
    Variable == "Plant_family" ~ "Family taxonomic rank of the observed plant species",
    Variable == "Plant_genus" ~ "Genus taxonomic rank of the observed plant species",
    Variable == "Plant_unsure_id" ~ "Binary category to indicate if the plant species name is unsure (Yes) or not (No)",
    Variable == "Plant_uncertainty_type" ~ "If the name is unsure, type of species uncertainty is provided",
    Variable == "Pollinator_original_name" ~ "Pollinator species name given by the authors",
    Variable == "Pollinator_accepted_name" ~ "Harmonized pollinator species name in the database",
    Variable == "Pollinator_rank" ~ "Taxonomic rank of the observation",
    Variable == "Pollinator_order" ~ "Order taxonomic rank of the observed pollinator species",
    Variable == "Pollinator_family" ~ "Family taxonomic rank of the observed pollinator species",
    Variable == "Pollinator_genus" ~ "Genus taxonomic rank of the observed pollinator species",
    Variable == "Pollinator_unsure_id" ~ "Binary category to indicate if the pollinator species name is unsure (Yes) or not (No)",
    Variable == "Pollinator_uncertainty_type" ~ "If the name is unsure, type of species uncertainty is provided",
    TRUE ~ "Other"
  ))

#Next step add it to an R markdown
