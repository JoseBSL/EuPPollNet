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
summarise(Family_interactions = as.integer(sum(Interaction))) %>% 
ungroup() 

#Get number of species per family
plant_fam_species = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name, Interaction) %>% 
select(Plant_family, Plant_accepted_name) %>% 
group_by(Plant_family) %>% 
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
#Visualize tree INTERACTIONS-CIRCULAR
ggtree(tree_family_interactions, layout='circular') + 
geom_tippoint(aes(colour=Family_species), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_color_gradientn(name = "",colours=c("red", 'orange', 'green', 'cyan', 'blue')) 
#Visualize tree INTERACTIONS-VERTICAL
ggtree(tree_family_interactions) + 
geom_tippoint(aes(colour=Family_species), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_color_gradientn(name = "",colours=c("red", 'orange', 'green', 'cyan', 'blue')) 






