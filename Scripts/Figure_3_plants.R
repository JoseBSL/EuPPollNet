
library(data.table) #To read long dataset (much faster with fread)
library(rgbif)
library(dplyr)
library(stringr)
library(rgbif)
library(ggtree)
library(rtrees)
library(ggplot2)
library(ggtreeExtra)
library(ggnewscale)

#Load plant distribution data
plants <- fread("Data/Species_taxonomy/Thesaurus/wcvp_distribution.csv")                                 
#Read interaction data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

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
taxPl <- fread("Data/Species_taxonomy/Thesaurus/wcvp_names.csv")                                 


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
#Wind pollinated families (or Hydrophilous) AND families without EU distribution
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
                  "Cupressaceae", "Cymodoceaceae",
                  "Eucommiaceae", "Francoaceae",
                  "Pinaceae", "Ephedraceae")
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
#Check total number of species
taxPl_final %>% select(Plant_name) %>% n_distinct()
plant_name_col = taxPl_final %>% 
select(Plant_name) %>% distinct() %>% 
pull()

#Filter out wind poll families
taxPl_final = taxPl_final %>% 
filter(is.na(Pollination))

#Sample 1 spp per family
spp_list = taxPl_final %>% 
group_by(family) %>% 
slice_sample(n = 1) %>% 
select(Plant_name, genus, family, order) %>% 
rename(species = Plant_name,
       genus = genus,
       family = family) 


#Retrieve phylogenetic info to build tree
#Retrieve phylogenetic info
phylo_output <- get_tree(sp_list = spp_list,  
                         taxon = "plant")

#Visualize tree INTERACTIONS-CIRCULAR
ggtree(phylo_output, layout='circular',
       ladderize = T, size=0.1) 

#Obtain approximate number of species per family
spp_per_family_Europe = taxPl_final %>%  
group_by(family, order) %>% 
summarise(Spp_Europe = n_distinct(Plant_name))


#Rename spp from phylo to family level
l = tibble(species = str_replace_all(phylo_output$tip.label, "_", " "))
l1 =left_join(l, spp_list)
phylo_output$tip.label = l1$family
#Get database plant data
#Interactions
plant_fam_interactions = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name, Interaction) %>% 
select(Plant_family, Interaction) %>% 
group_by(Plant_family) %>% 
summarise(Interactions = log(1+as.integer(sum(Interaction)))) %>% 
ungroup() 

#Get number of species per family
plant_fam_species = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name,Plant_order, Interaction) %>% 
select(Plant_family, Plant_accepted_name, Plant_order) %>% 
group_by(Plant_family) %>% 
summarise(Spp_SafeNet = n_distinct(Plant_accepted_name)) %>% 
ungroup() 

#
safenet_data = left_join(plant_fam_interactions, plant_fam_species) %>% 
rename(family = Plant_family) %>% 
mutate(Group = "A")


all = full_join(spp_per_family_Europe, safenet_data, by = "family") 
#Drop families that are not matching, those are wind poll or gymnos
all = all %>% 
filter(!is.na(Spp_Europe))%>% 
rename(label = family) %>% 
#mutate(Interactions = case_when(is.na(Interactions) ~ 0, T ~ Interactions)) %>% 
mutate(Spp_SafeNet = case_when(is.na(Spp_SafeNet) ~ 0, T ~ Spp_SafeNet)) %>% 
mutate(Group = case_when(is.na(Group) ~ "B", T ~ Group))

#Prepare dataset with percentage of European and missing 
d = all %>% 
mutate(Percent = 100 - Spp_SafeNet/Spp_Europe*100) %>% 
mutate(Percent = case_when(Spp_SafeNet == 0 ~ 100, T ~Percent)) %>% 
select(Group, Percent, label)


a = d %>% mutate(Group = "A")
b = d %>% 
mutate(Percent1 = 100-Percent) %>% 
mutate(Group = "B") %>% 
select(!Percent) %>% 
rename(Percent = Percent1)

d1 = bind_rows(a,b) %>% 
rename(fam_group = Group)

d1$fam_group = factor(d1$fam_group, levels = c("A", "B"))


#Join tree with data
tree_family_interactions = full_join(phylo_output, all, by="label")



#Save data and plot it in an Rmarkdown
saveRDS(tree_family_interactions, "Data/Manuscript_info/plant_phylo.RData")
saveRDS(d1, "Data/Manuscript_info/plant_coverage_phylo_RData")


#Check if there are differences in labels and order
ggtree(tree_family_interactions, layout="circular", size=0.15, open.angle=5, alpha=0.25) +
geom_tippoint(aes(size= Interactions), fill = "#d0f0c0", color="black", shape=21) +
geom_tiplab(linetype='dashed', linesize=.05, offset = -11, size=1.85, fontface=2, hjust=1) +
scale_size(name = "log(Interactions)", range = c(-1,4)) +
#xlim(0, 450)  +
new_scale_fill() +
geom_fruit(data = d1, geom=geom_bar,
                    mapping=aes(y=label, x=Percent, fill = fam_group),
                    pwidth=0.1, 
                    orientation="y", 
                    stat="identity",
                    color="black",
                    offset = 0.023,
            axis.params=list(
                         axis = "x",
                         text.size  = 1.5,
                         hjust  = 1,
                         vjust = 0.5,
                         nbreak = 3,
                         fontface = 2
                     ),
         grid.params=list())+
guides(fill = "none")+
scale_fill_manual(values = c(A = "lightgrey", B = "#d0f0c0"), guide = "none") +
geom_fruit(geom=geom_bar,
                    mapping=aes(y=label, x=log(Spp_Europe +1)),
                    pwidth=0.1, 
                    orientation="y", 
                    stat="identity",
                    color="black",
                    offset = 0) 
