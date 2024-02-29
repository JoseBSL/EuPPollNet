#Code to explore graphically pollinator coverage of the different species

#Load libraris
library(data.tree)
library(ape)
library(phytools)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggtree)
library(ggpattern)

#------------------------------------------------------------------#
#Load data----
#------------------------------------------------------------------#
#Safenet interactions
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")
#Bee-syrphid Masterlist
master_list = read_csv("Data/Species_taxonomy/Thesaurus/Master_bees_syrphids.csv")
#Bee phylo
bee.trees=read.tree(file="Data/Working_files/phylogeny_genus_level.txt")

#------------------------------------------------------------------#
#Explore the number of interactions and species per bee family-----
#------------------------------------------------------------------#
#Bee families
bee_fam = c("Apidae", "Megachilidae", "Halictidae", 
            "Andrenidae", "Colletidae","Melittidae")
#Bee family interactions
bee_family_interactions = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Pollinator_order, Pollinator_family)%>%
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera") %>% 
filter(!Pollinator_accepted_name == "Apis mellifera") %>% 
filter(Pollinator_family %in% bee_fam) %>% 
group_by(Pollinator_family) %>% 
summarise(Interactions = length(Pollinator_family))
#Bee family species
bee_family_species = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Pollinator_order, Pollinator_family)%>%
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera") %>% 
filter(!Pollinator_accepted_name == "Apis mellifera") %>% 
filter(Pollinator_family %in% bee_fam) %>% 
group_by(Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name))
#------------------------------------------------------------------#
#Prepare tibble with bee species data
#------------------------------------------------------------------#
#Create tibble with bee information
bees = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Pollinator_order, Pollinator_family, Pollinator_genus)%>%
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera") %>% 
filter(!Pollinator_accepted_name == "Apis mellifera") %>% 
filter(Pollinator_family %in% bee_fam) %>% 
mutate(Pollinator_accepted_name = 
   str_replace(Pollinator_accepted_name, 
   "Nomiapis", "Pseudapis")) 
#------------------------------------------------------------------#
#Check bee coverage at European level------
#------------------------------------------------------------------#
#Unique species per genus
species = master_list %>% 
select("Order", "Family", "Subfamily", "Tribe", 
  "Genus", "Subgenus", "Species", "GenusAndSpecies", "TaxonomicAuthority") %>% 
rename(Pollinator_family = Family,
       Pollinator_accepted_name = GenusAndSpecies) %>% 
filter(Pollinator_family %in% bee_fam) %>% 
group_by(Genus) %>%
slice_sample(n = 1) %>%
mutate(Pollinator_accepted_name = str_replace(Pollinator_accepted_name, " ", "_")) %>% 
filter(!grepl("Tarsalia", Pollinator_accepted_name)) %>% 
filter(!grepl("Cubiandrena", Pollinator_accepted_name)) %>% 
filter(!grepl("Metadioxys", Pollinator_accepted_name)) %>% 
filter(!grepl("Ensliniana", Pollinator_accepted_name)) %>% 
filter(!grepl("Schmiedeknechtia", Pollinator_accepted_name)) %>% 
dplyr::pull(Pollinator_accepted_name)
#Bee genera
bee_genera = master_list %>% 
select("Order", "Family", "Subfamily", "Tribe", 
  "Genus", "Subgenus", "Species", "GenusAndSpecies", "TaxonomicAuthority") %>% 
rename(Pollinator_family = Family,
       Pollinator_accepted_name = GenusAndSpecies) %>% 
filter(Pollinator_family %in% bee_fam) %>% 
select(Genus) %>%
filter(!Genus == "Tarsalia") %>% 
filter(!Genus == "Cubiandrena") %>% 
filter(!Genus == "Metadioxys") %>% 
filter(!Genus == "Ensliniana") %>% 
filter(!Genus == "Schmiedeknechtia") %>% 
distinct() %>% 
pull()


##Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)

check = tibble(bee.mcmc$tip.label)

d = tibble(spp = bee.mcmc$tip.label)
v = d %>%  
filter(!spp %in% bee_genera) %>% 
pull()
bee.tree100=drop.tip(bee.mcmc, tip = v)
bee.tree100$tip.label

#add dummy species labels
bee.tree100$tip.label<-paste(bee.tree100$tip.label,"_dum",sep="")
#Add species tips
for(i in 1:length(species)){
    bee.tree100<-add.species.to.genus(bee.tree100,species[i],
                                      where="root")
}

## prune out dummy taxa
ii<-grep("dum",bee.tree100$tip.label)
bee.tree100<-drop.tip(bee.tree100,bee.tree100$tip.label[ii])
#Our tree
plot(bee.tree100, cex = 0.6)

#------------------------------------------------------------------#
#Explore missing species and edit phylo tree
#------------------------------------------------------------------#
##Check for missing species
s = setdiff(species,bee.tree100$tip.label)
s
#To check nodes where to add the sister group that is missing
ggtree(bee.tree100, size=0.1,) + 
geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=1.75) + 
geom_tiplab(size=2)+
theme(plot.margin = unit(c(2,2,2,2),"cm")) +
coord_cartesian(clip="off")

#Add missing genera
#Select just the genus of the phylo tree
bee.tree100$tip.label = word(str_replace(bee.tree100$tip.label, "_", " "),1)  
#Add Seladonia
common_ancestor = getMRCA(bee.tree100, c("Halictus", "Lasioglossum"))
bee.tree100 = bind.tip(bee.tree100, "Seladonia", edge.length=NULL, where=common_ancestor, position=0)
#Rename Pseudapis
bee.tree100$tip.label = str_replace(bee.tree100$tip.label, "Pseudapis", "Nomiapis")
#Add Seladonia
common_ancestor = getMRCA(bee.tree100, c("Panurgus", "Melitturga"))
bee.tree100 = bind.tip(bee.tree100, "Simpanurgus", edge.length=NULL, where=common_ancestor, position=0)
#Add Parammobatodes
common_ancestor = getMRCA(bee.tree100, c("Pasites", "Ammobates"))
bee.tree100 = bind.tip(bee.tree100, "Parammobatodes", edge.length=NULL, where=common_ancestor, position=0)
#Add Parammobatodes
common_ancestor = getMRCA(bee.tree100, c("Pasites", "Ammobates"))
bee.tree100 = bind.tip(bee.tree100, "Chiasmognathus", edge.length=NULL, where=common_ancestor, position=0)

## Function for adding a cherry to a tree where a single tip was before
add.cherry <- function(tree, tip, new.tips) {
    ## Find the edge leading to the tip
    tip_id <- match(tip, tree$tip.label)
    ## Create the new cherry
    tree_to_add <- ape::stree(length(c(tip, new.tips)))
    ## Naming the tips
    tree_to_add$tip.label <- c(tip, new.tips)
    ## Add 0 branch length
    tree_to_add$edge.length <- NULL
    ## Binding both trees
    return(bind.tree(tree, tree_to_add, where = tip_id))
}
## Adding a new sister taxon with NULL branch length
bee.tree100 = add.cherry(bee.tree100, tip = "Nomioides", new.tips = "Ceylalictus")
bee.tree100 = add.cherry(bee.tree100, tip = "Rophites", new.tips = "Rhophitoides")
bee.tree100 = add.cherry(bee.tree100, tip = "Panurgus", new.tips = c("Halopanurgus", "Flavipanurgus"))
bee.tree100 = add.cherry(bee.tree100, tip = "Ancyla", new.tips = "Tarsalia")
bee.tree100 = add.cherry(bee.tree100, tip = "Stelis", new.tips = "Ensliniana")
bee.tree100 = add.cherry(bee.tree100, tip = "Panurgus", new.tips = c("Halopanurgus", "Flavipanurgus"))

#Now prepare data for plotting
#Create tibble with all be genera and left join the data
#Bee genera
bee_genera1 = master_list %>% 
select("Order", "Family", "Subfamily", "Tribe", 
  "Genus", "Subgenus", "Species", "GenusAndSpecies", "TaxonomicAuthority") %>% 
rename(Pollinator_family = Family,
       Pollinator_accepted_name = GenusAndSpecies) %>% 
select(Pollinator_family, Genus)
filter(Pollinator_family %in% bee_fam) %>% 
filter(!Genus == "Tarsalia") %>% 
filter(!Genus == "Cubiandrena") %>% 
filter(!Genus == "Metadioxys") %>% 
filter(!Genus == "Ensliniana") %>% 
filter(!Genus == "Schmiedeknechtia") %>% 
mutate(Genus = str_replace(Genus, "Pseudapis", "Nomiapis")) %>% 
group_by(Genus, Pollinator_family) %>% 

distinct()

bee_genera1 = tibble(label = bee_genera1)
#Genus level
genus_level = bees %>% 
group_by(Pollinator_genus, Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name),
          Interactions = log(length(Pollinator_accepted_name)+1)) %>% 
rename(label = Pollinator_genus)

left_join(bee_genera1, genus_level)
