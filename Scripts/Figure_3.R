#Load libraries
library(rtrees)
library(dplyr)
library(ape)
library(ggtree)
library(ggplot2)
library(stringr)


#Read interaction data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#----------------------------#
#1.1)Prepare species list-----
#----------------------------#
#Generate dataset with species, genus and family
#for retreving phylo
spp_list = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_accepted_name,Plant_genus, Plant_family) %>% 
rename(species = Plant_accepted_name,
       genus = Plant_genus,
       family = Plant_family) %>% 
distinct() 

#----------------------------#
#1.2)Download phylo-----
#----------------------------#
#Retrieve phylogenetic info
phylo_output <- get_tree(sp_list = spp_list,  
                         taxon = "plant")
#----------------------------#
#1.3)Select spp abundances and interactions-----
#----------------------------#
#Interactions
plant_spp_interactions = data %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
select(Plant_rank, Plant_status,Plant_accepted_name, Interaction) %>% 
select(Plant_accepted_name, Interaction) %>% 
group_by(Plant_accepted_name) %>% 
summarise(n = as.integer(sum(Interaction))) %>% 
ungroup()

#----------------------------#
#1.4)Merge-----
#----------------------------#
#Merge interaction data and phylogenetic information
spp_level_interactions <- data.frame(label=phylo_output$tip.label, var2=log(plant_spp_interactions$n))
tree_interactions <- full_join(phylo_output, spp_level_interactions, by='label')
#----------------------------#
#1.5)Plot-----
#----------------------------#
#Visualize tree INTERACTIONS-CIRCULAR
ggtree(tree_interactions, layout='circular',
       ladderize = T, size=0.1) %<+% plant_spp_abundance  +
#geom_text(aes(label=node), hjust=-.3)+ +
geom_tippoint(aes(color=var2), size=1) +
scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) 
#Visualize tree INTERACTIONS-VERTICAL
ggtree(tree_interactions) + 
geom_tippoint(aes(colour=var2)) + 
scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) 

#REPEAT PROCESS AT FAMILY LEVEL
#----------------------------#
#2.1)Prepare family list-----
#----------------------------#
#Filter out gymnosperm families

gymnos = c("Pinaceae", "Cupressaceae", "Ephedraceae")
#create spp list
spp_list_family = spp_list %>% 
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>% 
filter(!family %in% gymnos)
#----------------------------#
#2.2)Download phylo-----
#----------------------------#
phylo_family <- get_tree(sp_list = spp_list_family,  
                         taxon = "plant")

#----------------------------#
#2.3)Select spp interactions-----
#----------------------------#
to_fix = tibble(species = phylo_family$tip.label) %>% 
mutate(species = str_replace(species, "_", " ")) %>% 
mutate(species = str_replace(species, "_", " "))
to_fix1 = left_join(to_fix, spp_list_family)
phylo_family$tip.label = to_fix1$family

#Interactions
plant_fam_interactions = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name, Interaction) %>% 
select(Plant_family, Interaction) %>% 
group_by(Plant_family) %>% 
summarise(Family_interactions = log(as.integer(sum(Interaction + 1)))) %>% 
ungroup() 


#Get number of species per family
plant_fam_species = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name,Plant_order, Interaction) %>% 
select(Plant_family, Plant_accepted_name, Plant_order) %>% 
group_by(Plant_family, Plant_order) %>% 
summarise(Family_species = log(n_distinct(Plant_accepted_name))) %>% 
ungroup() 

#----------------------------#
#1.4)Merge-----
#----------------------------#
#Merge interaction data and phylogenetic information
family_level = tibble(Plant_family=phylo_family$tip.label)

family_level = left_join(family_level, plant_fam_interactions)  %>% 
left_join(plant_fam_species) %>% 
rename(label = Plant_family)
                      
tree_family_interactions <- full_join(phylo_family, family_level, by='label')



#----------------------------#
#1.5)Plot-----
#----------------------------#
plant_fam_interactions = plant_fam_interactions %>% 
mutate(Plant_family1 = Plant_family) %>% 
mutate(Family_interactions1 = Family_interactions)

# The circular layout tree.
p <- ggtree(tree_family_interactions, layout="fan", size=0.1, open.angle=5, alpha=0.5)+ 
geom_tippoint(aes(size= Family_species), colour='cyan4') +
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = -36, fontface=2)
library(ggnewscale)
library(ggtreeExtra)

p1 = p + new_scale_fill() +
         geom_fruit(data=plant_fam_interactions, geom=geom_bar,
                    mapping=aes(y=Plant_family1, x=Family_interactions1),
                    pwidth=0.38, 
                    orientation="y", 
                    stat="identity", 
                    color = "black") +
theme(legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

#----------------------------#
#PREPARE GRAPH WITH FAMILY COVERAGE-----
#----------------------------#

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
plants <- fread("Data/Species_taxonomy/Thesaurus/wcvp_distribution.csv")                                 

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
                    "Urticaceae",  "Zosteraceae")
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

data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#Generate data
#Interactions
plant_fam_interactions = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name, Interaction) %>% 
select(Plant_family, Interaction) %>% 
group_by(Plant_family) %>% 
summarise(Family_interactions = as.integer(sum(Interaction))) %>% 
ungroup() 


#Get number of species per family
plant_fam_species = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name,Plant_order, Interaction) %>% 
select(Plant_family, Plant_accepted_name, Plant_order) %>% 
group_by(Plant_family, Plant_order) %>% 
summarise(Family_species = n_distinct(Plant_accepted_name)) %>% 
ungroup() 

#Merge interaction data and phylogenetic information
family_level = left_join(plant_fam_species, plant_fam_interactions)  %>% 
rename(label = Plant_family)


d = taxPl_final %>% 
group_by(family) %>% 
summarise(spp_family_europe = n_distinct(Plant_name)) 
  
d1 = family_level %>% 
rename(family = label)
  
  
check = left_join(d, d1) %>% 
arrange(desc(Family_species)) %>% 
slice(1:50)

a = check %>%  
select(family, spp_family_europe) %>% 
rename(Family_names = family,
   Spp_per_family = spp_family_europe) %>%
mutate(Group = "Europe")

b = check %>%  
select(family, Family_species) %>% 
rename(Family_names = family,
   Spp_per_family = Family_species) %>% 
mutate(Group = "SafeNet")

c = bind_rows(a,b)

#Save data to plot it in markdown file
saveRDS(c, "Data/Working_files/Figures/data_figure3b.rds")
saveRDS(tree_family_interactions, "Data/Working_files/Figures/data_figure3a1.rds")
saveRDS(plant_fam_interactions, "Data/Working_files/Figures/data_figure3a2.rds")

library(ggplot2)
p2 = ggplot() + 
geom_col(data = c, 
  aes(x = reorder(Family_names, Spp_per_family), 
  y = log(Spp_per_family), fill = Group), position=position_dodge(0.1),width=1) +
xlab(NULL) +
ylab("log(Species)") +
theme_bw() +
theme(axis.text.y = element_text(size = 8, face = "bold"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
coord_flip(expand = F) +
scale_fill_manual(values = c("black", "cyan4")) +
  theme(plot.margin = margin(-10, 0, -10, -10, "pt")) +
ggtitle("b)")


# The circular layout tree.
p <- ggtree(tree_family_interactions, layout="fan", size=0.1, open.angle=5, alpha=0.5)+ 
geom_tippoint(aes(size= Family_species), colour='cyan4') +
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = -36, fontface=2) +
labs("log(Species)")+
scale_size(name = "log(Species)") +
ggtitle("a)") +
 theme(plot.title = element_text(vjust = -30, hjust = 0.1))
library(ggnewscale)
library(ggtreeExtra)

p1 = p + new_scale_fill() +
         geom_fruit(data=plant_fam_interactions, geom=geom_bar,
                    mapping=aes(y=Plant_family1, x=Family_interactions1),
                    pwidth=0.38, 
                    orientation="y", 
                    stat="identity", 
                    color = "black") +
theme(legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-50,-50,-50,-50))+
  theme(plot.margin = margin(-10, -20, -10, -10, "pt")) 



library(cowplot)
p2_1 = plot_grid(NULL,p2,NULL, ncol = 1, rel_heights  = c(0.5, 2, 0.5),rel_widths  = c(0.1, 2, 0.1))
plot_grid(p1,p2_1, rel_widths = c(20, 6), rel_heights  = c(8, 1), align = "none")
