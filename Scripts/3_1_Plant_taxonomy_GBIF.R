#------------------------------------------------#
#Clean plant taxonomy
#------------------------------------------------#

#Workflow:
#1) Load plant spp from the full metaweb
#2) Retrieve spp taxonomy from GBIF
#3) Separate matched and unmatched records
#4) Manual checks of unmatched records-and fix-
#5) Retrieve data for unmatched records again
#6) Fix manually what is still not found and add uncertainty
#7) Some last edits according to worldflora 
#7) see script 3_Plant_taxonomy_Worldflora.R
#8) Save plant taxonomy
#9) Safety checks by study ID

#Note:
#Additional datasets can be checked by filtering their spp
#They will require a manual check
#Only Study_id needs to be updated
#Check this at the end of the code

#Load libraries
library(dplyr)
library(rgbif)

#--------------------------------------#
#1) Load plant spp-----
#--------------------------------------#
#Read raw metaweb
master = readRDS("Data/Processing/Building_metaweb.rds")
#Load function to fix str of data
source("Scripts/Processing/Functions/Change_str.R")

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

#--------------------------------------#
#2) Retrieve GBIF taxonomy-----------
#--------------------------------------#
#Download taxonomic info from GBIF
gbif_names = name_backbone_checklist(name= name, kingdom='plants')
#Organise structure of data
gbif_names = change_str1(gbif_names) 

#--------------------------------------#
#3) Separate matched and unmatched records-----
#--------------------------------------#
#Match and unmatched records
matched = gbif_names %>% filter(Matchtype == "EXACT")
unmatched = gbif_names %>% filter(Matchtype != "EXACT")

#--------------------------------------#
#4) Manual checks of unmatched and fix-------
#--------------------------------------#
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
                " female", " male", " spec.$", " NA$",
                " sp. $")
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
  Fixed_name == "Leontopodium alpinum" ~ "Leontopodium nivale", #typo
  Fixed_name == "Rosa rubignosa" ~ "Rosa rubiginosa", #typo
  Fixed_name == "Thymus serphyllum" ~ "Thymus serpyllum", #typo
  Fixed_name == "Hypochoeris radicata" ~ "Hypochaeris radicata", #typo
  Fixed_name == "Stachys officinalis" ~ "Betonica officinalis", #synonym
  Fixed_name == "Stachys alopecuros" ~ "Betonica alopecuros", #synonym
  Fixed_name == "Cirsium montanum" ~ "Cirsium monspessulanum", #synonym
  Fixed_name == "Hieracium piloselloides" ~ "Pilosella piloselloides", #synonym
  str_detect(Fixed_name,"Senecio jacob") ~ "Jacobaea vulgaris", #synonym
  Fixed_name == "Convolvulus sepium" ~ "Calystegia sepium", #synonym
  Fixed_name == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis", #synonym
  str_detect(Fixed_name,"Odontites sero") ~ "Odontites vulgaris", #synonym
  Fixed_name == "Inula montana" ~ "Pentanema montanum", #synonym
  Fixed_name == "Scilla autumnalis" ~ "Scilla verna", #synonym
  Fixed_name == "Aster linosyris" ~ "Galatella linosyris", #synonym
  Old_name == "Gymnadenia conopcea" ~ "Gymnadenia conopsea", #typo
  Fixed_name == "Senecio abrotanifolius" ~ "Jacobaea abrotanifolia", #synonym
  Fixed_name == "Helianthemum alpestre" ~ "Helianthemum oelandicum", #synonym
  Fixed_name == "Chrysanthemum segetum" ~ "Glebionis segetum", #synonym
  Fixed_name == "Orchis morio" ~ "Anacamptis morio", #synonym
  Fixed_name == "Ficaria verna" ~ "Ranunculus ficaria", #synonym
  str_detect(Fixed_name,"Chrysanthemum seg") ~ "Glebionis segetum", #synonym
  Fixed_name == "Lonicera pileata" ~ "Lonicera ligustrina", #synonym
  Fixed_name == "Raphanus sativus" ~ "Raphanus raphanistrum", #synonym
  Fixed_name == "Senecio bicolor" ~ "Jacobaea maritima", #synonym
  str_detect(Fixed_name,"Leontodon autum") ~ "Scorzoneroides autumnalis", #synonym
  Fixed_name == "Inula britannica" ~ "Pentanema britannicum", #synonym
  Fixed_name == "Peucedanum longifolium" ~ "Peucedanum officinale", #synonym
  Fixed_name == "Centaurea pseudophrygia" ~ "Centaurea phrygia", #synonym
  Fixed_name == "Leontodon L." ~ "Leontodon", #fix
  Fixed_name == "Lentodon sp." ~ "Leontodon", #fix
  Fixed_name == "Whitania frutescens" ~ "Withania frutescens", #fix
  Fixed_name == "Compuesta amarilla de hoja peluda" ~ "Asteraceae", #fix
  Fixed_name == "Compuesta amarilla de hoja no peluda" ~ "Asteraceae", #fix
  Fixed_name == "Trifolium amarillo" ~ "Trifolium", #fix
  Fixed_name == "Trifolium blanco" ~ "Trifolium", #fix
  Fixed_name == "Lupinus micranthus" ~ "Lupinus gussoneanus", #fix


  T ~ Fixed_name)) %>% 
mutate(Fixed_name =  gsub("[0-9]+", "", Fixed_name)) %>% 
mutate(Fixed_name =  str_replace(Fixed_name, 
pattern =  paste(pattern_spp, collapse = "|"), "")) 

#--------------------------------------#
#5) Retrieve data for unmatched records again-----
#--------------------------------------#

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

#--------------------------------------#
#6) Fix manually what is still not found and add uncertainty------
#--------------------------------------#
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


#Add some final accepted names that are not being included
Plant_data1 = Plant_data1 %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Asteraceae" ~ "Asteraceae",
  Fixed_name == "Hieracium" ~ "Hieracium",
  T ~ Accepted_name
))


#--------------------------------------#
#7) Some last edits according to worldflora-----
#--------------------------------------#
#Fix records for merging with master list
#They are ok (GBIF) but they are not merging
#with worldflora masterlist
Plant_data1 = Plant_data1 %>% 
mutate(Accepted_name = case_when(
Accepted_name == "Achyrophorus valdesii" ~
  "Hypochaeris achyrophorus", #Internal Accepted name
Accepted_name == "Candollea elata" ~ 
"Pseudopodospermum elatum",#Internal Accepted name
Accepted_name == "Sinapis arvensis" ~ 
"Brassica arvensis", #Internal Accepted name
Accepted_name == "Mentha piperita" ~ 
"Mentha × piperita", #Internal Accepted name
Accepted_name == "Papaver alpinum" ~ 
"Oreomecon alpina", #Internal Accepted name
Accepted_name == "Mycelis muralis" ~ 
"Lactuca muralis", #Internal Accepted name
Accepted_name == "Poterium sanguisorba" ~ 
"Sanguisorba minor", #Internal Accepted name
Accepted_name == "Trommsdorffia uniflora" ~ 
"Hypochaeris maculata", #Internal Accepted name
Accepted_name == "Inula salicina" ~ 
"Pentanema salicinum", #Internal Accepted name
Accepted_name == "Chamaenerion dodonaei" ~ 
"Epilobium dodonaei", #Internal Accepted name
Accepted_name == "Bryonia dioica" ~ 
"Bryonia cretica", #Internal Accepted name
Accepted_name == "Brassica nigra" ~ 
"Brassica juncea", #Internal Accepted name
Accepted_name == "Oreoselinum nigrum" ~ 
"Peucedanum oreoselinum", #Internal Accepted name
Accepted_name == "Symphytum uplandicum" ~ 
"Symphytum × uplandicum", #Internal Accepted name
Accepted_name == "Hieracium vulgatum" ~ 
"Hieracium lachenalii", #Internal Accepted name
Accepted_name == "Anemone nemorosa" ~ 
"Anemonoides nemorosa", #Internal Accepted name
Accepted_name == "Rhinanthus serotinus" ~ 
"Rhinanthus major", #Internal Accepted name
Accepted_name == "Lycopsis arvensis" ~ 
"Anchusa arvensis", #Internal Accepted name
Accepted_name == "Medicago varia" ~ 
"Medicago × varia", #Internal Accepted name
Accepted_name == "Vaccinium hybridum" ~ 
"Vaccinium × hybridum", #Internal Accepted name
Accepted_name == "Lotus delortii" ~ 
"Lotus corniculatus", #Internal Accepted name
Accepted_name == "Halimium calycinum" ~ 
"Cistus calycinus", #Internal Accepted name
Accepted_name == "Halimium halimifolium" ~ 
"Cistus halimifolius", #Internal Accepted name
Accepted_name == "Sixalix atropurpurea" ~ 
"Scabiosa atropurpurea", #Internal Accepted name
Accepted_name == "Thrincia hispida" ~ 
"Leontodon longirostris", #Internal Accepted name
Accepted_name == "Bellidiastrum michelii" ~ 
"Aster bellidiastrum", #Internal Accepted name
Accepted_name == "Mutellina adonidifolia" ~ 
"Mutellina purpurea", #Internal Accepted name
Accepted_name == "Valerianella locusta" ~ 
"Valeriana locusta", #Internal Accepted name
Accepted_name == "Anemone sylvestris" ~ 
"Anemonoides sylvestris", #Internal Accepted name
Accepted_name == "Poterium verrucosum" ~ 
"Sanguisorba verrucosa", #Internal Accepted name
Accepted_name == "Bellardia viscosa" ~ 
"Parentucellia viscosa", #Internal Accepted name
Accepted_name == "Centranthus calcitrapae" ~ 
"Valeriana calcitrapae", #Internal Accepted name
Accepted_name == "Thrincia saxatilis" ~ 
"Leontodon saxatilis", #Internal Accepted name
Accepted_name == "Spiraea arguta" ~ 
"Spiraea × arguta", #Internal Accepted name
Accepted_name == "Leucanthemum superbum" ~ 
"Leucanthemum × superbum", #Internal Accepted name
Accepted_name == "Laburnum watereri" ~ 
"Laburnum × watereri", #Internal Accepted name
Accepted_name == "Centranthus ruber" ~ 
"Valeriana rubra", #Internal Accepted name
Accepted_name == "Petunia atkinsiana" ~ 
"Valeriana rubra", #Internal Accepted name
Accepted_name == "Begonia semperflorens" ~ 
"Begonia cucullata", #Internal Accepted name
Accepted_name == "Crocosmia crocosmiiflora" ~ 
"Crocosmia × crocosmiiflora", #Internal Accepted name
Accepted_name == "Lonicera pileata" ~ 
"Lonicera ligustrina", #Internal Accepted name
Accepted_name == "Lonicera pileata" ~ 
"Lonicera ligustrina", #Internal Accepted name
Accepted_name == "Spiraea billardii" ~ 
"Spiraea × billardii", #Internal Accepted name
Accepted_name == "Parthenocissus inserta" ~ 
"Parthenocissus quinquefolia", #Internal Accepted name
Accepted_name == "Amelanchier lamarckii" ~ 
"Amelanchier × lamarckii", #Internal Accepted name
Accepted_name == "Pentanema britannicum" ~ 
"Pentanema britannica", #Internal Accepted name
Accepted_name == "Aesculus carnea" ~ 
"Aesculus × carnea", #Internal Accepted name
Accepted_name == "Dombeya cayeuxii" ~ 
"Dombeya × cayeuxii", #Internal Accepted name
Accepted_name == "Galactites tomentosa" ~ 
  "Galactites tomentosus", #Accepted name
Accepted_name == "Sedum candollei" ~ 
  "Sedum candolleanum", #Accepted name
Accepted_name == "Fagonia cretica" ~ 
  "Zygophyllum creticum", #Accepted name
T ~ Accepted_name))


#Final edit, as some genera have changed
#we select the first word of the changed ones
#and add it back to the genera
Plant_data1 = Plant_data1 %>% 
mutate(Genus = 
    if_else(Genus == word(Plant_data1$Accepted_name, 1), 
            Genus, word(Plant_data1$Accepted_name, 1)))

#Similar case
#Genus level records are not added as accepted
Plant_data1 = Plant_data1 %>% 
mutate(Accepted_name = 
if_else(is.na(Accepted_name) & Matchtype== "EXACT" & Rank =="GENUS", 
            Canonical_name, Accepted_name))

#--------------------------------------#
#8) Save pollinator taxonomy-------
#--------------------------------------#
#check colnames
colnames(Plant_data1)
saveRDS(Plant_data1, "Data/Species_taxonomy/Plant_taxonomy.rds")

#--------------------------------------#
#9) Bind data to conduct final safety checks
#--------------------------------------#
master = master %>%  
rename(Old_name = Plant_species)
#Merge
all = left_join(master, Plant_data1)
#Quick subset to explore the new plants added
#Do this fo every new dataset that we add
#Last one being checked is written within the filter argument
subset_check = all %>% 
filter(Study_id == "50_Hervias-Parejo") %>% 
select(Old_name, Fixed_name, Rank, Status, Matchtype, Accepted_name, Unsure_id) %>% 
distinct()

