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
#Create tibble with abundances
plant_spp_abundance = data %>% 
select(Plant_status, Plant_rank,Pollinator_accepted_name, Plant_accepted_name) %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
select(Plant_accepted_name) %>% 
count(Plant_accepted_name) %>% 
arrange(-n) %>% 
rename(tip.label = Plant_accepted_name) %>% 
mutate(tip.label = str_replace(tip.label, " ", "_"))

#Top 20 plants with higher interactions
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
#Merge abundance data and phylogenetic information
spp_level_abundances <- data.frame(label=phylo_output$tip.label, var1=log(plant_spp_abundance$n))
tree_abundances <- full_join(phylo_output, spp_level_abundances, by='label')
#Merge interaction data and phylogenetic information
spp_level_interactions <- data.frame(label=phylo_output$tip.label, var2=log(plant_spp_interactions$n))
tree_interactions <- full_join(phylo_output, spp_level_interactions, by='label')
#----------------------------#
#1.5)Plot-----
#----------------------------#
#Visualize tree ABUNDANCE-CIRCULAR
ggtree(tree_abundances, layout='circular',
       ladderize = T, size=0.1) %<+% plant_spp_abundance  +
#geom_text(aes(label=node), hjust=-.3)+ +
geom_tippoint(aes(color=var1), size=1) +
scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) 
#Visualize tree ABUNDANCE-VERTICAL
ggtree(tree_abundances) + 
geom_tippoint(aes(colour=var1)) + 
scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) 

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
#2.3)Select spp abundances-----
#----------------------------#
to_fix = tibble(species = phylo_family$tip.label) %>% 
mutate(species = str_replace(species, "_", " "))
to_fix1 = left_join(to_fix, spp_list_family)
phylo_family$tip.label = to_fix1$family
#Abundances
plant_fam_abundance = data %>% 
select(Plant_status, Plant_rank,Pollinator_accepted_name, Plant_accepted_name, Plant_family) %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_family) %>% 
count(Plant_family) %>% 
arrange(-n) %>% 
rename(tip.label = Plant_family) %>% 
filter(!tip.label %in% gymnos)

#Interactions
plant_fam_interactions = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_status,Plant_family,Plant_accepted_name, Interaction) %>% 
select(Plant_family, Interaction) %>% 
group_by(Plant_family) %>% 
summarise(n = as.integer(sum(Interaction))) %>% 
ungroup() %>% 
rename(tip.label = Plant_family) %>% 
filter(!tip.label %in% gymnos)

#----------------------------#
#1.4)Merge-----
#----------------------------#
#Merge abundance data and phylogenetic information
family_level_abundances <- data.frame(label=phylo_family$tip.label, var1=log(plant_fam_abundance$n))
tree_family_abundances <- full_join(phylo_family, family_level_abundances, by='label')
#Merge interaction data and phylogenetic information
family_level_interactions <- data.frame(label=phylo_family$tip.label, var2=log(plant_fam_interactions$n))
tree_family_interactions <- full_join(phylo_family, family_level_interactions, by='label')
#----------------------------#
#1.5)Plot-----
#----------------------------#
#Visualize tree ABUNDANCES-CIRCULAR
ggtree(tree_family_abundances, layout='circular') + 
geom_tippoint(aes(colour=var1), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_color_gradientn(name = "",colours=c("red", 'orange', 'green', 'cyan', 'blue')) 
#Visualize tree ABUNDANCES-VERTICAL
ggtree(tree_family_abundances) + 
geom_tippoint(aes(colour=var1), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_color_gradientn(name = "",colours=c("red", 'orange', 'green', 'cyan', 'blue')) 
#Visualize tree INTERACTIONS-CIRCULAR
ggtree(tree_family_interactions, layout='circular') + 
geom_tippoint(aes(colour=var2), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_color_gradientn(name = "",colours=c("red", 'orange', 'green', 'cyan', 'blue')) 
#Visualize tree INTERACTIONS-VERTICAL
ggtree(tree_family_interactions) + 
geom_tippoint(aes(colour=var2), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_color_gradientn(name = "",colours=c("red", 'orange', 'green', 'cyan', 'blue')) 


#Quick check before the meeting
#SPECIES LEVEL
d = left_join(spp_level_abundances,spp_level_interactions)
cor.test(d$var1,d$var2)
library(ggpubr)
ggscatter(d, x = "var1", y = "var2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson") +
xlab("Species abundances") + 
ylab("Species interactions")

#FAMILY LEVEL
d = left_join(family_level_abundances,family_level_interactions)
cor.test(d$var1,d$var2)
library(ggpubr)
ggscatter(d, x = "var1", y = "var2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson") +
xlab("Fam. abundance") + 
ylab("Fam interactions")


#TO CHECK
#Convert phylogenetic tree into matrix
A <- vcv.phylo(phylo_family)
#Standardize to max value 1
A_standardized <- A/max(A)
#Add phylo column to dataset
dat_analysis$phylo
dat_analysis$phylo <- dat_analysis$Species_all
str(dat_analysis)

