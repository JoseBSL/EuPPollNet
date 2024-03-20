#----------------------------#
#In this script we extract the European species from worldflora
#and check against our list of spp standardise from GBIF
#We do final edits to match the taxonomy from worldflora
#In order to know the completeness of our dataset
#----------------------------#
#Workflow:
#1) Read from worldflora and fiter
#2) Flag wind pollinated taxonomic groups
#3) Read GBIF species from the final file AND
#3) Check matching names with worldflora masterlist
#4) We edit on the GBIF script for synonyms 
#4) when accepted on GBIF and are on the masterlist
#4) when not, we look for the species in the unfiltered masterlist
#4) and add it manually by species code
#5) check completeness
#NOTE1: These last cases are generally exotic species
#from private gardens, bot gardens, etc
#We include them for now as the focus are pollinators
#but think about exclude those from the final metaweb
#NOTE2: Further work at subspecies level could be done

#Load libraries
library(dplyr)
library(rgbif)
library(stringr)
library(data.table) #To read long dataset (much faster with fread)

#----------------------------#
#1) Read from worldflora and fiter----
#----------------------------#
#Master list downloaded from:
#https://kew.iro.bl.uk/concern/datasets/32f77ea6-0f7b-4b2d-b7b3-173ed4ca2d6a?locale=en
#DOI: 10.34885/jdh2-dr22

#Load plant distribution data
plants = fread("Data/Species_taxonomy/Thesaurus/wcvp_distribution.csv")                                 

#check cols
colnames(plants)
#check continents
levels(factor(plants$continent))
levels(factor(plants$area))
levels(factor(plants$location_doubtful))
levels(factor(plants$area))
levels(factor(plants$introduced))

#Filter by continent, and minor checks
plants1 = plants %>% 
filter(continent == "EUROPE") %>% 
filter(!area == "") %>% #filter unknown location
filter(!extinct == "1") %>% #filter extinct spp
filter(!location_doubtful == "1") #doubtful location

colnames(plants1)
#Select cols of interest
plants2 = plants1 %>% 
select(!c(continent_code_l1, region_code_l2, area_code_l3,
          introduced, extinct, location_doubtful))

#Load plant sepecies name data
taxPl = fread("Data/Species_taxonomy/Thesaurus/wcvp_names.csv")                                 


#check cols
colnames(taxPl)
#Check levels
levels(factor(taxPl$taxon_status))
levels(factor(taxPl$taxon_rank))
#Filter 
taxPl1 = taxPl %>%
filter(plant_name_id %in% plants1$plant_name_id) %>% 
filter(taxon_status == "Accepted") %>% 
filter(!taxon_rank == "Genus") 
#Additions after checking taxonomy
spp = c("Pseudopodospermum elatum",
        "Allium sipyleum",
        "Allium exile",
        "Valerianella vesicaria")
taxPl_excluded = taxPl %>%
filter(taxon_name %in% spp)

#----------------------------#
#4) Read from worldflora and fiter----
#----------------------------#
#Number 4 as this informed by unmatched names, 
#checked in section number 3

#By code to evitate duplicates
#Many of these plants are not located naturally in Europe
#Grown in gardens or plantations mostly
#Or some locations that are at European borders
spp_code = c("2682000",#Brassica arvensis
             "3082805", #Jacobaea vulgaris
             "2715606", #Chamaenerion angustifolium
             "419046",  #Holcus lanatus
             "3254094", #Tilia europaea
             "2848854", #Heuchera sanguinea
             "3137644", #Taraxacum officinale
             "3250793", #Hypochaeris maculata
             "3083264", #Senecio rupestris
             "207106",  #Trachelospermum jasminoides
             "182583",  #Salvia elegans
             "3221681", #Eriolarynx australis
             "2436154", #Thunbergia grandiflora
             "351575",  #Jasminum mesnyi
             "2571650", #Polygala fruticosa
             "3261485", #Abelia chinensis
             "385642",  #Juniperus communis
             "2961916", #Prunus spicata
             "3270532", #Leontodon taraxacoides
             "3211261", #Taraxacum vulgare
             "183314",  #Salvia nubicola (records mainly out of Europe)
             "3091231", #Helichrysum serotinum
             "2992660", #Spiraea × arguta
             "3112787", #Leucanthemum × superbum
             "2419624", #Rhododendron davidi
             "2549681", #Petunia × atkinsiana
             "2691834", #Calibrachoa parviflora
             "3079049", #Brachyglottis greyi
             "361362",  #Begonia cucullata
             "3087100", #Dimorphotheca ecklonis
             "2992744", #Spiraea billardii
             "2624566", #Aesculus parviflora
             "82267",   #Euphorbia spathulata
             "182630",  #Salvia farinacea
             "182380",  #Salvia coccinea
             "2461992", #Veronicastrum virginicum
             "4499",    #Agastache urticifolia
             "2763338", #Deutzia scabra
             "3131133",  #Liatris pycnostachya
             "31419",   #Callistemon citrinus
             "2530300", #Philadelphus incanus
             "2992747", #Spiraea blumei
             "113859",   #Liriodendron tulipifera
             "2624483",   #Aesculus × carnea
             "2466437",   #Wisteria floribunda
             "2529835",   #Persea americana
             "117853",    #Magnolia stellata
             "2446614",  #Umbellularia californica
             "2610698",  #Abutilon indicum
             "117537",   #Magnolia denudata
             "2358497",  #Mahonia japonica
             "2458511",  #Viburnum suspensum
             "2694618",  #Camellia japonica
             "2774894",  #Dombeya × cayeuxii
             "202449",   #Tetrapanax papyrifer
             "2866020",  #Iochroma fuchsioides
             "456152",   #Albuca bracteata
             "2694880", #Camellia sinensis,
             "3211150", #Taraxacum levigatum
             "3075260", #Picris angustifolia
             "3290297", #Candollea mollis 
             "3074512" #Hypochaeris cretensis
             ) 
#Save those plants from unfiltered master file
taxPl_excluded1 = taxPl %>%
filter(plant_name_id %in% spp_code)
#Add plants back to processing file
taxPl1 = bind_rows(taxPl1, taxPl_excluded, taxPl_excluded1)
taxPl1 = taxPl1 %>% 
select(!c(taxon_status, ipni_id, accepted_plant_name_id, 
          basionym_plant_name_id, replaced_synonym_author,
          homotypic_synonym, parent_plant_name_id, 
          powo_id, hybrid_formula, reviewed))
#Generate species name
taxPl2 = taxPl1 %>% 
mutate(Plant_name = case_when(
str_detect(genus_hybrid, "\\×")  ~ paste(genus_hybrid, genus, species),
str_detect(species_hybrid, "\\×")  ~ paste(genus, species_hybrid, species),
T ~ paste(genus, species)))
#Now we can get rid of the hybrid cols (and others)
taxPl3 = taxPl2 %>% 
select(!c(genus_hybrid, species_hybrid,
          infraspecific_rank, parenthetical_author,
          primary_author, publication_author,
          place_of_publication, volume_and_page,
          first_published, nomenclatural_remarks,
          taxon_authors))
#Bind datasets
all = left_join(plants2, taxPl3) %>% 
filter(!is.na(Plant_name)) #exclude not matching records

#----------------------------#
#2) Flag wind pollinated taxonomic groups----
#----------------------------#
#To filter easier we download from GBIF taxonomy
#order and class
name = taxPl3 %>% select(family) %>% distinct() %>%  pull()
family_data = name_backbone_checklist(name= name, kingdom='plants')
#Select main cols for merging back
family_data1 = family_data %>% 
select(c(verbatim_name, class, order)) %>% 
rename(family = verbatim_name)
#Bind new info to the dataset
taxPl4 = left_join(taxPl3, family_data1)

#These species are classify as wind poll
#but they show interactions with floral visitors
wind_with_interaction =c("Juniperus communis", 
                         "Larix decidua")
spp_out = taxPl4 %>% 
filter(Plant_name %in% wind_with_interaction)
#We select only the angiosperm classes
#check classes
levels(factor(taxPl4$class))
#1st filter
out_class = c("Ginkgoopsida", "Lycopodiopsida", 
              "Pinopsida", "Polypodiopsida")
#Gnetopsida check as some as insect poll
#We keep it
taxPl4 = taxPl4 %>% 
filter(!class %in% out_class)
#Add missed spp from these classes
taxPl4 = bind_rows(taxPl4, spp_out)

#Check orders (now all are angios)
#We flag wind pollination in an extra col
levels(factor(taxPl4$order))
#Add orders to filter out
out_order = c("Gunnerales", "Ceratophyllales")
#Add wind poll label
taxPl4 = taxPl4 %>% 
mutate(Pollination = case_when(
  str_detect(order, pattern = paste(out_order, collapse = "|")) ~ "wind_poll",
  T ~ NA_character_
))
#Flag now wind poll families from Poales
out_poales = c("Poaceae", "Cyperaceae", "Juncaceae",
                 "Typhaceae", "Typhaceae")
taxPl4 = taxPl4 %>% 
mutate(Pollination = case_when(
  str_detect(family, pattern = paste(out_poales, collapse = "|")) ~ "wind_poll",
  T ~ Pollination
))

#Other families
#Wind pollinated families (or Hydrophilous)
#(checked manually by me with the species on the list)
#Some key papers helped: 
#https://doi.org/10.1016/S0169-5347(02)02540-5
wind_poll_fam = c("Altingiaceae", "Amaranthaceae",
                    "Betulaceae", "Cannabaceae", 
                    "Casuarinaceae", "Cercidiphyllaceae",
                    "Coriariaceae", "Cymodoceaceae",
                    "Datiscaceae", "Eucommiaceae",
                    "Haloragaceae", "Hamamelidaceae",
                    "Juglandaceae", "Juncaginaceae",
                    "Myricaceae", "Nothofagaceae",
                    "Platanaceae", "Posidoniaceae",
                    "Potamogetonaceae", "Ruppiaceae",
                    "Scheuchzeriaceae", "Ulmaceae",
                    "Urticaceae",  "Zosteraceae",
                  "Ephedraceae")
#Flag wind poll families
taxPl4 = taxPl4 %>% 
mutate(Pollination = case_when(
  str_detect(family, pattern = paste(wind_poll_fam, collapse = "|")) ~ "wind_poll",
  T ~ Pollination
))
#Now flag wind poll genera
wind_poll_gen = c("Hippophae", "Fagus", "Quercus",
                  "Najas", "Elodea", "Halophila",
                  "Vallisneria", "Hydrilla", "Lagarosiphon",
                  "Maclura", "Morus", "Phillyrea",
                  "Olea", "Fraxinus", "Callitriche",
                  "Littorella", "Hippuris", "Populus",
                  "Callitriche", "Rumex")
#Flag wind poll genera
taxPl4 = taxPl4 %>% 
mutate(Pollination = case_when(
  str_detect(genus, pattern = paste(wind_poll_gen, collapse = "|")) ~ "wind_poll",
  T ~ Pollination
))
#Some random wind pollinated species found
#Not doing a proper check at species level
#https://doi.org/10.1093/aob/mcy131
wind_poll_spp = c("Thalictrum alpinum", "Thalictrum foetidum",
                  "Thalictrum macrocarpum")
#Flag wind poll genera
taxPl4 = taxPl4 %>% 
mutate(Pollination = case_when(
  str_detect(Plant_name, pattern = paste(wind_poll_spp, collapse = "|")) ~ "wind_poll",
  T ~ Pollination
))

#Final fixes of species names to homogenise
#This is done with the help of worldflora and GBIF
taxPl_final = taxPl4 

#Minor edit on masterlist
taxPl_final = taxPl_final %>% 
mutate(Plant_name = case_when(taxon_rank == "Species" & Plant_name == "Knautia drymeja" ~ "Knautia drymeia",
                           T ~ Plant_name))
#Check total number of species
taxPl_final %>% select(Plant_name) %>% n_distinct()
plant_name_col = taxPl_final %>% 
select(Plant_name) %>% distinct() %>% 
pull()

#----------------------------#
#3) Read GBIF species ----
#----------------------------#
#Red plant taxonomy and check % of coverage
data = readRDS("Data/Species_taxonomy/Plant_taxonomy.rds")
colnames(data)

plant_spp = data %>% 
select(Rank, Status, 
Matchtype, Accepted_name) %>% 
filter(Rank == "SPECIES")%>% 
select(Accepted_name)    

#Gnenerate unique cases from master list
plant_spp_list = plant_spp %>% 
distinct() 
#Check for mismatches
mismatches = plant_spp_list %>% 
filter(!Accepted_name %in% plant_name_col) 

#If the value is 0, we can check the completeness now :)
mismatches1 = mismatches %>% pull()
mismatches1

#We could build something that looks for the unmatched 
#And add the corrected ones (at the moment is done manually)

#Check if 0 unmatched records
plant_checks = ifelse(nrow(mismatches) == 0, 
       "All plants match masterlist", 
       "There are mistmatches of plants")

plant_checks

#----------------------------#
#5) check completeness----
#----------------------------#
#Check species without wind poll
#But first exclude the recorded ones
#As there are few that are considered wind poll
#with interactions

#Create lis of plant species
plant_spp_list = plant_spp %>% 
n_distinct() 
plant_spp_list

#Generate list
plant_spp = plant_spp %>% 
distinct() %>% pull()
#Separate matched and unmatched
one = taxPl_final %>%
filter(Plant_name %in% plant_spp)
two = taxPl_final %>%
filter(!Plant_name %in% plant_spp)
#Exclude wind poll from two
no_wind_poll = two %>%
filter(is.na(Pollination)) 
#Bind excluded matched records back
all_filtered = bind_rows(no_wind_poll, one)
#Select unique cases
all_spp_filtered = all_filtered %>%
select(Plant_name) %>% 
n_distinct()
#Calculate percentage
#It doesn't change much as there are only few records
#that are wind poll with interactions recorded
total_coverage_non_wind = 
  plant_spp_list/all_spp_filtered *100
total_coverage_non_wind

saveRDS(total_coverage_non_wind, "Data/Manuscript_info/plant_coverage.RData")

#Remember to trust this if:
plant_checks

