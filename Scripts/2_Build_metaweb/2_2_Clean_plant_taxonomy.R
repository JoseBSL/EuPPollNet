########################################################################
#2) Clean plant taxonomy----
########################################################################

#Workflow:
#1) Load plant spp from the full metaweb
#2) Retrieve spp taxonomy from GBIF
#3) Separate matched and unmatched records
#4) Manual checks of unmatched records-and fix-
#5) Retrieve data for unmatched records again
#6) Fix manually what is still not found
#7) Plant taxonomy should be ready

#Note:
#Additional datasets can be checked by filtering their spp
#They will require a manual check
#Only Study_id needs to be updated
#Check this at the end of the code

#Load libraries
library(dplyr)

#########
#1) Load plant spp from the full metaweb
#########
#Read raw metaweb
master = readRDS("Data/Processing/Building_metaweb.rds")
#Load function to fix str of data
source("Scripts/Change_str.R")

#Explore plant data
#Check total number of distinct plant species names
n_distinct(master$Plant_species)
#Check levels
s = tibble(levels(factor(master$Plant_species)))

#Create plant spp dataset
Plant_spp = master %>% 
select(Plant_species) %>% 
distinct() %>%
rename(name = Plant_species) 
#Extract spp names in a vector
name = Plant_spp %>% pull()

#########
#2) Retrieve spp taxonomy from GBIF
#########
#Download taxonomic info from GBIF
gbif_names = name_backbone_checklist(name= name, kingdom='plants')
#Organise structure of data
gbif_names = change_str1(gbif_names) 

#########
#3) Separate matched and unmatched records
#########
#Match and unmatched records
matched = gbif_names %>% filter(Matchtype == "EXACT")
unmatched = gbif_names %>% filter(Matchtype != "EXACT")

#########
#4) Manual checks of unmatched records-and fix-
#########

#4.1Check records with fuzzy matching
#From the unmatched records check first the FUZZY matchtype
unmatched_fuzzy = unmatched %>% filter(Matchtype == "FUZZY")
unmatched_higher_rank = unmatched %>% filter(Matchtype == "HIGHERRANK")
unmatched_none = unmatched %>% filter(Matchtype == "NONE")
#Scroll down in dataset and check for mistakes
#Manual fixes (FUZZY)
Plant_spp = Plant_spp %>% 
rename(Old_name = name) %>% 
mutate(Fixed_name = case_when(
  Old_name == "Heuchera cult" ~ "Heuchera",
  Old_name == "Rosa NA" ~ "Rosa",
  Old_name == "Rubus rubus" ~ "Rubus", #Write subgenus as genus level
  Old_name == "Cruciata verum" ~ "Galium verum", #I think they refer to G. verum
  Old_name == "Ranunculus tribbus" ~ "Ranunculus", #I think they refer to tribus rank
  T ~ Old_name
  ))

#Some patterns to be fixed
pattern_spp = c(" sp$"," sp.$", ".aggr$", " aggr.$", 
                " aggr$", " agg$", " agg.$", " cult$", 
                " female", " male", " spec.$", " NA$")
#Manual fixes (HIGHERRANK and NONE)
Plant_spp = Plant_spp %>% 
mutate(Fixed_name = case_when(
  Fixed_name == "Aetheorrhiza bulbosa" ~ "Aetheorhiza bulbosa", #typo
  Fixed_name == "Carpobrotus affinis acinaciformis" ~ "Carpobrotus acinaciformis",
  Fixed_name == "Sedum sediforme" ~ "Petrosedum sediforme", #synonym
  Fixed_name == "Scabiosa stellata" ~ "Lomelosia stellata", #synonym
  Fixed_name == "Epilobium angustifolium" ~ "Chamaenerion angustifolium", #synonym
  Fixed_name == "Inula conyzae" ~ "Pentanema squarrosum", #synonym
  Fixed_name == "Lythrum vulgare" ~ "Lythrum salicaria", #synonym
  Fixed_name == "Gypsophyla repens" ~ "Gypsophila repens", #typo
  Fixed_name == "Rhinantus glacialis" ~ "Rhinanthus glacialis", #typo
  Fixed_name == "Silene rupestris" ~ "Heliosperma alpestre", #synonym
  Fixed_name == "Scabiosa gramuntia" ~ "Pycnocomon intermedium", #synonym
  Fixed_name == "Silene alba" ~ "Silene latifolia", #synonym
  Fixed_name == "Polygonum viviparum" ~ "Bistorta vivipara", #synonym
  Fixed_name == "Senecio alpinus" ~ "Jacobaea alpina", #synonym
  Fixed_name == "Hypochaeris uniflora" ~ "Trommsdorffia uniflora", #synonym
  Fixed_name == "Senecio cordatus" ~ "Jacobaea alpina", #synonym
  Fixed_name == "Tanacetum coymbosum" ~ "Tanacetum corymbosum", #typo
  str_detect(Fixed_name, "Taraxacum sect") ~ "Taraxacum", #fix to higherrank
  Fixed_name == "Trifolium geel" ~ "Trifolium", #unknown, fix to higherank
  Fixed_name == "Papaver repens" ~ "Papaver", #unknown, fix to higherank
  Fixed_name == "Centaurea vulgare" ~ "Centaurea jacea", #very likely synonym
  Fixed_name == "Polygonum jacea" ~ "Polygonum", #unknown, fix to higherank
  Fixed_name == "Falcaria aviculare" ~ "Falcaria", #unknown, fix to higherank
  Fixed_name == "Achillea vulgaris" ~ "Achillea", #unknown, fix to higherank...
  Fixed_name == "Cirsium sativa" ~ "Cirsium", #unknown, fix to higherank...
  Fixed_name == "Hypericum reicheri" ~ "Hypericum richeri", #typo
  str_detect(Fixed_name, "Erica sp\\.2 cf\\. tetralix") ~ "Erica tetralix", #Add uncertainty later
  Fixed_name == "Narcissus cassata" ~ "Narcissus", #fix to higherank, cassata is kind of a section
  Fixed_name == "Matricaria inodora" ~ "Tripleurospermum inodorum", #synonym
  Fixed_name == "Photinia serrulata" ~ "Photinia serratifolia", #synonym
  Fixed_name == "Hieracium hieracium" ~ "Hieracium", #unknown, fix to higherank...
  Fixed_name == "Rhinanthus angustifolius" ~ "Rhinanthus serotinus", #synonym
  Fixed_name == "Lithospermum arvense" ~ "Buglossoides arvensis", #synonym
  Fixed_name == "Trientalis europaea" ~ "Lysimachia europaea", #synonym
  Fixed_name == "Galium corrudifolium" ~ "Galium lucidum", #synonym
  Fixed_name == "Ornithogalum angustifolium" ~ "Ornithogalum umbellatum", #synonym
  Fixed_name == "Gymnadenia conopcea" ~ "Gymnadenia conopea", #typo
  Fixed_name == "Senecio erucifolius" ~ "Jacobaea erucifolia", #synonym
  Fixed_name == "Gentiana ciliata" ~ "Gentianopsis ciliata", #synonym
  Fixed_name == "Senecio erucifoliaea" ~ "Jacobaea erucifolia", #synonym
  Fixed_name == "Ranunculus nemorosus" ~ "Ranunculus polyanthemos", #synonym
  Fixed_name == "Chrysanthemum vulgare" ~ "Tanacetum vulgare", #synonym
  Fixed_name == "Aster bellidiastrum" ~ "Bellidiastrum michelii", #synonym
  Fixed_name == "Mentha aquatilis" ~ "Mentha aquatica", #likely aquatica
  Fixed_name == "Rheum barbarum" ~ "Rheum rhabarbarum", #likely rhabarbarum
  Fixed_name == "Gallium mollugo" ~ "Galium mollugo", #typo
  Fixed_name == "Silene album" ~ "Silene latifolia", #synonym
  Fixed_name == "Heracleum saxifraga" ~ "Heracleum", #unknown, fix to higherank...
  Fixed_name == "Anthirrinum majus" ~ "Antirrhinum majus", #typo
  Fixed_name == "Bupleurum falsifica" ~ "Bupleurum", #unknown, fix to higherank...
  Fixed_name == "Cardamine draba" ~ "Lepidium draba", #I think is probably Lepidium...
  Fixed_name == "Cistus psilosepalus" ~ "Cistus inflatus", #synonym
  Fixed_name == "Sanguisorba verrucosa" ~ "Poterium verrucosum", #synonym
  Fixed_name == "Capsella bursa.pastoris" ~ "Capsella bursa-pastoris", #typo
  Fixed_name == "Genista rara" ~ "Genista", #unknown, fix to higherank...
  Fixed_name == "Cistus ladanifer_leaves" ~ "Cistus ladanifer", #typo
  Fixed_name == "Ranunculus auricomus coll" ~ "Ranunculus auricomus", #typo
  Fixed_name == "Leontodon saxatilis" ~ "Thrincia saxatilis", #synonym
  Fixed_name == "Rosa pimpernellifolia" ~ "Rosa spinosissima", #synonym
  Fixed_name == "Sysimbrium officinale" ~ "Sisymbrium officinale", #typo
  Fixed_name == "Escallonia macrantha" ~ "Escallonia rubra", #synonym
  Fixed_name == "Petunia xatkinsiana" ~ "Petunia ×atkinsiana", #typo
  Fixed_name == "Malus pumila" ~ "Malus domestica", #synonym
  Fixed_name == "Weigela kosteriana_variegata" ~ "Weigela", #unknown, fix to higherank...
  Fixed_name == "Polygonum lapathifolium" ~ "Persicaria lapathifolia", #synonym
  Fixed_name == "Lycopersicon esculentum" ~ "Solanum lycopersicum", #synonym
  Fixed_name == "Sedum hybridum" ~ "Phedimus hybridus", #synonym
  Fixed_name == "Hypericum X hidcote" ~ "Hypericum", #subgenus, fix to higherank...
  Fixed_name == "Centaurea ausiaca" ~ "Centaurea phrygia", #typo and fix to possible synonym
  Fixed_name == "Vicia angustifolia" ~ "Vicia sativa", #typo and fix to possible synonym
  Fixed_name == "Piantina viola" ~ "Viola", #Fix to genus (piantina is italian)
  Fixed_name == "Laythyrus sylvestris" ~ "Lathyrus sylvestris", #typo 
  Fixed_name == "Papaver Rhoeas" ~ "Papaver rhoeas", #typo
  Fixed_name == "Hypochaeris Radicata" ~ "Hypochaeris radicata", #typo
  Fixed_name == "?" ~ "Unknown", #unknown
  Fixed_name == "VIOLETTE" ~ "Viola", #fix name
  Fixed_name == "Vegetaliea" ~ "Unknown", #unknown
  Fixed_name == "Visum sativum" ~ "Lathyrus oleraceus", #typo and synonym
  Fixed_name == "NA NA" ~ "Unknown", #unknown
  Fixed_name == "Unknown Unknown" ~ "Unknown", #unknown
  Fixed_name == "Arthocteca calendula" ~ "Arctotheca calendula", #typo
  Fixed_name == "unknown flower" ~ "Unknown", #typo
  str_detect(Fixed_name, "Chichoriaceae") ~ "Taraxacum", #fix to higher rank
  Fixed_name == "Ameria maritima" ~ "Armeria maritima", #fix to higher rank
  str_detect(Fixed_name, "Fuschia") ~ "Fuchsia", #Typo
  Fixed_name == "Xheranthemum cylindraceum" ~ "Xeranthemum cylindraceum", #typo
  is.na(Fixed_name) ~ "Unknown", #unknown
  Fixed_name == "Andryala (integrifolia)" ~ "Andryala integrifolia", #typo

  T ~ Fixed_name)) %>% 
mutate(Fixed_name =  str_replace(Fixed_name, 
pattern =  paste(pattern_spp, collapse = "|"), "")) %>% 
mutate(Fixed_name =  gsub("[0-9]+", "", Fixed_name))

#Generate plant list again, 
#retrieve data and check if now everything works
name1 = Plant_spp %>% select(Fixed_name) %>% distinct() %>%  pull()
gbif_names1 = name_backbone_checklist(name= name1, kingdom='plants')
#Organise structure of data
gbif_names1 = change_str1(gbif_names1) 
#Check unmatched records again and see if fuzzy records are corrected
unmatched1 = gbif_names1 %>% filter(Matchtype != "EXACT")
unmatched_fuzzy1 = unmatched1 %>% filter(Matchtype == "FUZZY") 
unmatched_higherrank1 = unmatched1 %>% filter(Matchtype == "HIGHERRANK") 
unmatched_none1 = unmatched1 %>% filter(Matchtype == "NONE") 
#Looks good now
Plant_data = gbif_names1

#Add unsure_id col
unsure_id = c("[/]", "[?]", "Rosa canina agg.", "Rosa canina.aggr",
              "Erica sp.2 cf. tetralix", "Galium mollugo.aggr",
              "Rubus fruticosus.aggr", "Thymus serpyllum.aggr",
              "Rosa corymbifera.aggr", "Oenothera biennis.aggr")

Plant_spp1 = Plant_spp %>% 
mutate(Unsure_id = case_when(
str_detect(Old_name, pattern = paste(unsure_id, collapse = "|")) ~ "Yes",
Fixed_name == "Unknown" ~ "Yes",
T ~ "No"
))
#Now add Uncertainty_type
species_complex = c("[/]", "Rosa canina agg.", "Rosa canina.aggr",
              "Erica sp.2 cf. tetralix", "Galium mollugo.aggr",
              "Rubus fruticosus.aggr", "Thymus serpyllum.aggr",
              "Rosa corymbifera.aggr", "Oenothera biennis.aggr")

Plant_spp1 = Plant_spp1 %>% 
mutate(Uncertainty_type = case_when(
str_detect(Old_name, pattern = paste(species_complex, collapse = "|")) ~ "species_complex",
Old_name == "Erica sp.2 cf. tetralix" ~ "confer",
str_detect(Old_name, "[?]") ~ "unsure",
Fixed_name == "Unknown" ~ "unknown",
T ~ NA_character_
))

#Bind cols of uncertainty
Plant_data1 = left_join(Plant_data, Plant_spp1)
#############
#Save pollinator taxonomy
#############
#check colnames
colnames(Plant_data1)
saveRDS(Plant_data1, "Data/Species_taxonomy/Plant_taxonomy.rds")

#############
#Bind data to conduct final safety checks
#############
master = master %>%  
rename(Old_name = Plant_species)
#Merge
all = left_join(master, Plant_data1)
#Quick subset to explore the new plants added
#Do this fo every new dataset that we add
#Last one being checked is written within the filter argument
subset_check = all %>% 
filter(Study_id == "44_Knight") %>% 
select(Old_name, Fixed_name, Rank, Status, Matchtype, Accepted_name, Unsure_id) %>% 
distinct()
