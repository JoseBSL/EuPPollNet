#Load libraries
library(rtrees)
library(dplyr)
library(ape)
library(ggtree)
library(ggplot2)
#Read interaction data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")
#Check colnames
colnames(data)
#Generate dataset with species, genus and family
#for retreving phylo
spp_list = data %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
select(Plant_accepted_name,Plant_genus, Plant_family) %>% 
rename(species = Plant_accepted_name,
       genus = Plant_genus,
       family = Plant_family) %>% 
distinct() 





#Abundances
plant_spp_abundance = data %>% 
select(Plant_status, Plant_rank, Plant_accepted_name) %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
select(Plant_accepted_name) %>% 
count(Plant_accepted_name) %>% 
arrange(-n) %>% 
rename(tip.label = Plant_accepted_name) %>% 
mutate(tip.label = str_replace(tip.label, " ", "_"))


#Retrieve phylogenetic info
phylo_output <- get_tree(sp_list = spp_list,  
                         taxon = "plant")




#Visualise tree
ggtree(phylo_output, layout='circular',
       ladderize = T, size=0.1) %<+% plant_spp_abundance  +
#geom_text(aes(label=node), hjust=-.3)+ +
geom_tippoint(aes(color=n),
              size=2.25) +
    scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) 





set.seed(2020)
x <- phylo_output
d <- data.frame(label=phylo_output$tip.label, var1=log(plant_spp_abundance$n))
tree <- left_join(x, d, by='label')
trs <- list(TREE1 = tree)
class(trs) <- 'treedataList'
ggtree(trs) + 
geom_tippoint(aes(colour=var1)) + 
scale_colour_gradient(low='blue', high='red') 


ggtree(trs,ladderize = T, size=0.3) + 
geom_tree(aes(color=var1), continuous = 'colour', size=0.1) +  
scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) 



#Create phylo for families, select only 1 species

#Filter out gymnosperm families
gymnos = c("Pinaceae", "Cupressaceae", "Ephedraceae")

spp_list_family = spp_list %>% 
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>% 
filter(!family %in% gymnos)

phylo_family <- get_tree(sp_list = spp_list_family,  
                         taxon = "plant")

to_fix = tibble(species = phylo_family$tip.label) %>% 
mutate(species = str_replace(species, "_", " "))
to_fix1 = left_join(to_fix, spp_list_family)
phylo_family$tip.label = to_fix1$family
#Abundances
plant_fam_abundance = data %>% 
select(Plant_status, Plant_rank, Plant_accepted_name, Plant_family) %>% 
filter(Plant_status == "ACCEPTED" & Plant_rank == "SPECIES") %>% 
filter(!Plant_accepted_name =="Helianthus annuus") %>% 
select(Plant_family) %>% 
count(Plant_family) %>% 
arrange(-n) %>% 
rename(tip.label = Plant_family) %>% 
filter(!tip.label %in% gymnos)

set.seed(2020)
x <- phylo_family
d <- data.frame(label=phylo_family$tip.label, var1=log(plant_fam_abundance$n))
tree <- left_join(x, d, by='label')
trs <- list(TREE1 = tree)
class(trs) <- 'treedataList'
ggtree(trs) + 
geom_tippoint(aes(colour=var1), size= 0.8) + 
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", offset = 1, fontface=3)+
scale_colour_gradient(low='blue', high='red') +
ggplot2::xlim(0, 200)
