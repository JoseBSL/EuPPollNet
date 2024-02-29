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
#Bee species interactions
ggplot(bee_family_interactions, aes(reorder(Pollinator_family, -Interactions), Interactions)) +
geom_col() +
xlab(NULL) +
theme(axis.text.x = element_text(angle = 45))
#Bee family species
bee_family_species = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Pollinator_order, Pollinator_family)%>%
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera") %>% 
filter(!Pollinator_accepted_name == "Apis mellifera") %>% 
filter(Pollinator_family %in% bee_fam) %>% 
group_by(Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name))
#Bee species 
ggplot(bee_family_species, aes(reorder(Pollinator_family, -Spp_number), Spp_number)) +
geom_col() +
xlab(NULL) +
theme(axis.text.x = element_text(angle = 45))
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
#Create bee list
bee_list = master_list %>% 
select("Order", "Family", "Subfamily", "Tribe", 
  "Genus", "Subgenus", "Species", "GenusAndSpecies", "TaxonomicAuthority") %>% 
rename(Pollinator_family = Family,
       Pollinator_accepted_name = GenusAndSpecies) %>% 
filter(Pollinator_family %in% bee_fam) %>% 
group_by(Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name))

unique(factor(bee_list$Genus))

#Explore graphically spp per family
#Bee species 
ggplot(bee_list, aes(reorder(Pollinator_family, -Spp_number), Spp_number)) +
geom_col() +
xlab(NULL) +
theme(axis.text.x = element_text(angle = 45))
#Prepare data for plotting it together
bee_family_species = bee_family_species %>% 
mutate(Group = "A")
bee_list = bee_list %>% 
mutate(Group = "B")
bee_plotting = rbind(bee_family_species, bee_list)
#Bee family coverage
ggplot(bee_plotting, aes(reorder(Pollinator_family, -Spp_number), Spp_number, fill = Group)) +
geom_col(position = "dodge") +
xlab(NULL) +
theme(axis.text.x = element_text(angle = 45)) +
theme_bw() +
coord_cartesian(clip = "off", expand = FALSE)
#


bee_plotting$Group = factor(bee_plotting$Group, levels= c("B","A"))


#Save data
saveRDS(bee_plotting, "Data/Manuscript_info/bee_plotting.RData")
#Save plot
ggplot(data = bee_plotting, aes(reorder(Pollinator_family, -Spp_number), 
       Spp_number, fill = Group, pattern = Group)) +
geom_col_pattern(position = "dodge",color = "black", pattern_fill = "black") +
scale_pattern_manual(values = c(A = "stripe", B = "none"), labels= c(A = "SafeNet spp", B = "European spp")) +
scale_fill_manual(values = c(A = "tan2", B = "lightblue3"), labels= c(A = "SafeNet spp", B = "European spp")) + 
coord_cartesian(expand = FALSE) +
theme_bw() + 
ylab("Number of species") +
xlab("")+
ggtitle("Bee family coverage") +
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"))



#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
set.seed(35)
df <- data.frame(Class = factor(rep(c(1,2),times = 80), labels = c("Math","Science")),
                 StudyTime = factor(sort(sample(1:4, 16, prob = c(0.25,0.3,0.3,0.15), replace = TRUE)),labels = c("<5","5-10","10-20",">20")),
                 Nerd = factor(sapply(rep(c(0.1,0.3,0.5,0.8),c(30,50,50,30)), function(x)sample(c("Nerd","NotNerd"),size = 1, prob = c(x,1-x))),levels = c("NotNerd","Nerd")))
ggplot(data = df, aes(x = Class, fill = StudyTime, pattern = Nerd)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
  scale_pattern_manual(values = c(Nerd = "stripe", NotNerd = "none")) +
  labs(x = "Class", y = "Number of Students", pattern = "Nerd?") + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))




#------------------------------------------------------------------#
#Prepare data for bee phylogeny------
#------------------------------------------------------------------#
#Load extracted data
species = bees %>%  
mutate(Pollinator_accepted_name = str_replace_all(Pollinator_accepted_name, " ", "_")) %>% 
distinct() %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Seladonia", "Halictus"))  %>% 
mutate(Pollinator_accepted_name = 
         str_replace(Pollinator_accepted_name, 
         "Nomiapis", "Pseudapis")) %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Ceylalictus", "Halictus")) %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Flavipanurgus", "Panurgus")) %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Flavipanurgus", "Panurgus")) %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Rhophitoides", "Dufourea")) %>% 
group_by(Pollinator_genus) %>%
slice_sample(n = 1) %>%
dplyr::pull(Pollinator_accepted_name)


#------------------------------------------------------------------#
#Select and prepare phylo tree
#------------------------------------------------------------------#
#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)

#Select species that I don't have to d
spp_list = bees %>% 
select(Pollinator_genus) %>% 
mutate(Pollinator_genus = 
   str_replace(Pollinator_genus, 
   "Nomiapis", "Pseudapis")) %>% 
distinct() %>%  
pull()

#Select species that I don't have 
d = tibble(spp = bee.mcmc$tip.label)
v = d %>%  
filter(!spp %in% spp_list) %>% 
pull()
bee.tree100=drop.tip(bee.mcmc, tip = v)

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
setdiff(species,bee.tree100$tip.label)
#To check nodes where to add the sister group that is missing
ggtree(bee.tree100, size=0.1,) + 
geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=1.75) + 
geom_tiplab(size=1)+
theme(plot.margin = unit(c(2,2,2,2),"cm")) +
coord_cartesian(clip="off")
#Select just the genus of the phylo tree
bee.tree100$tip.label = word(str_replace(bee.tree100$tip.label, "_", " "),1)  
#Add Seladonia
common_ancestor = getMRCA(bee.tree100, c("Halictus", "Lasioglossum"))
bee.tree100 = bind.tip(bee.tree100, "Seladonia", edge.length=NULL, where=common_ancestor, position=0)
#Rename Pseudapis
bee.tree100$tip.label = str_replace(bee.tree100$tip.label, "Pseudapis", "Nomiapis")
#Add Flavipanurgus
common_ancestor = getMRCA(bee.tree100, c("Panurgus", "Panurginus"))
bee.tree100 = bind.tip(bee.tree100, "Flavipanurgus", edge.length=NULL, where=common_ancestor, position=0)
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


#Try to add some data to the phylogenetic tree:
#Species number
family_level = bees %>% 
group_by(Pollinator_genus, Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name),
          Interactions = log(length(Pollinator_accepted_name)+1)) %>% 
rename(label = Pollinator_genus)

family_level1 = bees %>% 
group_by(Pollinator_genus, Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name),
          Interactions = log(length(Pollinator_accepted_name)+1))%>% 
rename(Pollinator_family1= Pollinator_family) %>% 
rename(Interactions1= Interactions)

#Join with dataframe
tree_family_interactions <- full_join(bee.tree100, family_level, by="label")

#------------------------------------------------------------------#
#Save data
#------------------------------------------------------------------#
saveRDS(tree_family_interactions, "Data/Manuscript_info/bee_phylo.RData")
saveRDS(family_level1, "Data/Manuscript_info/bee_family_info.RData")
#------------------------------------------------------------------#
#Plot tree
#------------------------------------------------------------------#
ggtree(bee.tree100, layout="fan", size=0.1, open.angle=5, alpha=0.5)+ 
geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=1.75, face="bold") + 
#geom_tippoint(aes(size= Family_species), colour='cyan4') +
geom_tiplab(fontface=2)

#Check how to color by bee family
ggtree(tree_family_interactions, layout="fan", size=0.1, open.angle=5, alpha=0.5)+ 
  geom_tiplab(fontface=2, aes( label=node))+
geom_tippoint(aes(size= Spp_number), colour='cyan4') 




library(ggnewscale)
library(ggtreeExtra)


#Circular layour
p = ggtree(tree_family_interactions, layout="fan", size=0.1, open.angle=5, alpha=0.5)+ 
geom_tippoint(aes(size= Spp_number, fill = Pollinator_family), color="black", shape=21) +
geom_tiplab(linetype='dashed', linesize=.05, offset = -2.9, size=1.85, fontface=4) +
labs("log(Species)")+
scale_size(name = "Species") + guides(fill = "none")

p1 = p + new_scale_fill() +
         geom_fruit(data=family_level1, geom=geom_bar,
                    mapping=aes(y=Pollinator_genus, x=Interactions1, fill = Pollinator_family1),
                    pwidth=0.38, 
                    orientation="y", 
                    stat="identity",
                    color="black",
                    offset = 0.05) +
theme(legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,-20,-20,-20))+
  theme(plot.margin = margin(-10, -20, -10, -10, "pt")) 
p1


#Horizontal layout
#Circular layour
p = ggtree(tree_family_interactions,  size=0.2,  alpha=1)+ 
geom_tippoint(aes(size= Spp_number, fill = Pollinator_family), color="black", shape=21) +
geom_tiplab(linetype='dashed', linesize=.05, offset = 0, size=1.85, fontface=4) +
labs("log(Species)")+
scale_size(name = "Species") + guides(fill = "none")

p1 = p + new_scale_fill() +
         geom_fruit(data=family_level1, geom=geom_bar,
                    mapping=aes(y=Pollinator_genus, x=Interactions1, fill = Pollinator_family1),
                    pwidth=0.2, 
                    orientation="y", 
                    stat="identity",
                    color="black",
                    offset = 0.08) 
p1
