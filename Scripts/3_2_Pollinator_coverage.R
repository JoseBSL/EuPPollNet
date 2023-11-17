#----------------------------#
#Total pollinator coverage
#----------------------------#

#Load libraries
library(readr)
#Prepare list of bee species to compare against it
#Load pollinator taxonomy
master_list = read_csv(file = "Data/Species_taxonomy/Thesaurus/Master_bees_syrphids.csv")
#Check cols
colnames(master_list)
#Select cols of interest
master_list = master_list %>% 
select(Order, Family, Subfamily, Tribe, Genus, Subgenus, Species,
       GenusAndSpecies, TaxonomicAuthority)

#First filter only key bee families
#There are 7 but just 6 in Europe
bee_fam = c("Apidae", "Megachilidae", "Halictidae",
            "Andrenidae", "Colletidae", "Melittidae")
#Create bee list
potential_bees = master_list %>% 
filter(Family %in% bee_fam) %>% 
select(GenusAndSpecies) 
  
#Edit some species names in master list

  
potential_bees1 = potential_bees %>%   
pull()

#Now load data and prepare the observed list of bees
data = readRDS("Data/Species_taxonomy/Pollinator_taxonomy.rds")
#Check colnames
colnames(data)
#Select cols of interest
poll_cols = data %>% 
select(Rank, Status, Accepted_name, Order, Family, Genus)
#Select be families
observed_bees = poll_cols %>% 
filter(Family %in% bee_fam)
#Now select only accepted species
observed_bees = observed_bees %>% 
select(Rank, Accepted_name) %>% 
filter(Rank == "SPECIES") %>% 
select(Accepted_name) %>% 
distinct() 
#Create vector with unique cases
unique_obs_bees =  observed_bees %>% pull()

#Now both lists are ready
#First get an overall idea
#Then, do proper checks with mathcing names

#Raw check of coverage
length(unique_obs_bees) / length(potential_bees1) * 100

#Now check by matching names
matched_bees = observed_bees %>% 
filter(Accepted_name %in% potential_bees1)
unmatched_bees = observed_bees %>% 
filter(!Accepted_name %in% potential_bees1)
#Good! 

