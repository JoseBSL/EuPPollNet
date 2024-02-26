#Code to explore graphically pollinator coverage of the different species


#Load libraris
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(ape)
library(phytools)
#Read data----
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")


#Bee fam
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



bees = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Pollinator_order, Pollinator_family, Pollinator_genus)%>%
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera") %>% 
filter(!Pollinator_accepted_name == "Apis mellifera") %>% 
filter(Pollinator_family %in% bee_fam) 

unique(factor(bees$Pollinator_genus))


#Bee species coverage (European level)
#Load master list
master_list = read_csv("Data/Species_taxonomy/Thesaurus/Master_bees_syrphids.csv")

#Create bee list
bee_list = master_list %>% 
select("Order", "Family", "Subfamily", "Tribe", 
  "Genus", "Subgenus", "Species", "GenusAndSpecies", "TaxonomicAuthority") %>% 
rename(Pollinator_family = Family,
       Pollinator_accepted_name = GenusAndSpecies) %>% 
filter(Pollinator_family %in% bee_fam) %>% 
group_by(Pollinator_family) %>% 
summarise(Spp_number = n_distinct(Pollinator_accepted_name))
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
theme(axis.text.x = element_text(angle = 45))

#Get phylo for pollinators
#Load phylogenetic tree GENUS LEVEL
#Generate Species vector 

#Load extracted data
species = bees %>%  
mutate(Pollinator_accepted_name = str_replace_all(Pollinator_accepted_name, " ", "_")) %>% 
distinct() %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Seladonia", "Halictus"))  %>% 
#mutate(Pollinator_accepted_name = 
#         str_replace(Pollinator_accepted_name, 
#         "Nomiapis", "Nomia")) %>% 
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
#Read phylo
bee.trees=read.tree(file="Data/Working_files/phylogeny_genus_level.txt")
#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species

#Add genus that I don't have 
bee.tree100=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                       "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                       "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                       "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                       "Ctenocolletes", "Alocandrena", "Megandrena",      
                                       "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                       "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                       "Perdita", "Clavipanurgus",        
                                       "Protandrena", "Pseudopanurgus",  
                                       "Arhysosage", "Callonychium", "Cerceris",        
                                       "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                       "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                       "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                       "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                       "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                       "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                       "Samba", "Capicola", "Hesperapis",      
                                       "Eremaphanta", "Redivivoides",    
                                       "Rediviva", "Promelitta", "Meganomia",       
                                       "Deltoptila", "Pachymelus",        
                                       "Sphecodopsis", "Oreopasites",     
                                       "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                       "Doeringiella", "Thalestria", "Triopasites",     
                                       "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                       "Hexepeolus", "Neolarra", "Biastes",         
                                       "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                       "Tetralonioidella", "Zacosmia", "Xeromelecta",         
                                       "Hopliphora", "Mesoplia", "Mesocheira",      
                                       "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                       "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                       "Melectoides", "Leiopodus", "Coelioxoides",    
                                       "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                       "Peponapis", "Xenoglossa", "Tetraloniella",   
                                       "Svastra", "Martinapis",      
                                       "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                       "Diadasia", "Alepidosceles", "Diadasina",       
                                       "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                       "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                       "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                       "Aglae", "Eulaema", "Eufriesea",            
                                       "Tetragonilla", "Tetragonula", "Platytrigona",    
                                       "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                       "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                       "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                       "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                       "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                       "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                       "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                       "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                       "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                       "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                       "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                       "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                       "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                       "Ctenoplectra", "Macrogalea", "Allodapula",      
                                       "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                       "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                       "Allodape", "Fideliopsis", "Fidelia",         
                                       "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                       "Noteriades", "Radoszkowskiana", 
                                       "Pseudoheriades", "Afroheriades",       
                                       "Stenoheriades", "Othinosmia",      
                                       "Haetosmia", "Wainia", "Hoplosmia",           
                                       "Ashmeadiella", "Atoposmia", "Stenosmia",       
                                        "Ochreriades",    
                                       "Serapista", "Bathanthidium",   
                                       "Dianthidium", "Paranthidium",  
                                       "Pachyanthidium", "Benanthis",     
                                       "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                       "Notanthidium", "Epanthidium",       
                                       "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                       "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                       "Macronomia", "Cellariella",     
                                       "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                       "Xenochlora", "Megaloptidia",    
                                       "Dinagapostemon", "Rhinetula",       
                                       "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                       "Eupetersia", "Mexalictus", "Patellapis",      
                                       "Thrincohalictus", "Homalictus",   
                                       "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                       "Xeralictus", "Protodufourea",       
                                       "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                       "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                       "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                       "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                       "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                       "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                       "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                       "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                       "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                       "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                       "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                       "Hemicotelles", "Mourecotelles", 
                                       "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                       "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                       "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                       "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                       "Chilicola", "Duckeanthidium",
                                       "Tachysphex","Apis",
                                       "Agapostemon","Augochlora",
                                       "Calliopsis", "Melissodes",
                                       "Ptilothrix", "Seladonia" 
))


plot(bee.tree100)
nodelabels()
tiplabels()

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

##Check for missing species
setdiff(species,bee.tree100$tip.label)

#
bee.tree100$tip.label = word(str_replace(bee.tree100$tip.label, "_", " "),1)  
  
  
#Add polytommies
#function to add new tips
add.cherry <- function(tree, tip, new.tips) {
  ## Find the edge leading to the tip
  tip_id <- match(tip, tree$tip.label)
  ## Create the new cherry
  tree_to_add <- ape::stree(length(c(tip, new.tips)))
  ## Naming the tips
  tree_to_add$tip.label <- c(tip, new.tips)
  ## Add 0 branch length
  tree_to_add$edge.length <- rep(5, Nedge(tree_to_add))
  ## Binding both trees
  return(bind.tree(tree, tree_to_add, where = tip_id))
}
tips_to_drop= c("Ceylalictus")
bee.tree100 = drop.tip(bee.tree100, tips_to_drop)
new_sister_species <- tips_to_drop
bee.tree100 = add.cherry(bee.tree100, tip = "Halictus", new.tips = new_sister_genus)




#Seldaonia:Halictus, Nomiapis:Nomis, Flavipanurgus:Panurgus,
#Ceylalictus:Halictus, Rhophitoides: Dufourea
#Those genera are included into these other as they are not in the tree

library(ggtree)

ggtree(bee.tree100, layout="fan", size=0.1, open.angle=5, alpha=0.5)+ 
#geom_tippoint(aes(size= Family_species), colour='cyan4') +
geom_tiplab(linetype='dashed', linesize=.05, 
      size=1.75, color= "black", fontface=2)

