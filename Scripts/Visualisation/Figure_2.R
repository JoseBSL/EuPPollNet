#Load libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(patchwork)

#Read interaction data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#Check cols
colnames(data)
#Select cols of interest
data1 = data %>% 
select(Study_id, Network_id, Authors_habitat,
  Corine_land_cover, SafeNet_habitat, Bioregion,
  Latitude, Longitude, Day, Month, Year, Plant_rank,
  Plant_status, Plant_matchtype, Plant_accepted_name,Plant_order,
  Pollinator_rank, Pollinator_status, Pollinator_matchtype,
  Pollinator_accepted_name, Pollinator_order, Interaction)
#Check cols again
colnames(data1)


#----------------------------
#Explore number of studies conducted in each habitat type and bioregion
#----------------------------
#Check number of studies by habitat
habitat = data %>% 
select(Study_id, SafeNet_habitat) %>% 
distinct() %>%  
group_by(SafeNet_habitat) %>% 
summarise(Habitat_count = n()) 
#Explore graphically
ggplot(habitat, aes(fct_reorder(SafeNet_habitat,-Habitat_count), Habitat_count)) +
geom_col(fill = "gray20") +
theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
ylab("Number of studies") +
xlab(NULL)

#Check number of studies by bioregion
bioregion = data %>% 
select(Study_id, Bioregion) %>% 
distinct() %>%  
group_by(Bioregion) %>% 
summarise(Bioregion_count = n()) 

#Explore graphically
ggplot(bioregion, aes(fct_reorder(Bioregion,-Bioregion_count), Bioregion_count)) +
geom_col(fill = "gray20") +
theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
ylab("Number of studies") +
xlab(NULL)


#----------------------------
#Calculate MAIN Poll orders by habitat type
#----------------------------
#Now try to explore pollinator orders by habitat 
main_orders = c("Hymenoptera", "Lepidoptera", "Coleoptera", "Diptera")

pollinator_percent = data1 %>%
group_by(SafeNet_habitat, Pollinator_order) %>%
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
filter(Pollinator_order %in% main_orders) %>% 
summarise(count = n()) %>%
group_by(SafeNet_habitat) %>%
mutate(Proportion = count/sum(count)) %>%
select(-count) 
#Plot orders
habitat_by_poll =  ggplot(pollinator_percent, 
aes(fill=Pollinator_order, y=Proportion, x=SafeNet_habitat)) + 
geom_bar(position="stack", stat="identity")+
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL)

#----------------------------
#Plot by habitat type
#----------------------------
#Left_join
habitat_poll = left_join(pollinator_percent, habitat)
#Order dataset
habitat_poll = habitat_poll %>% arrange(-Habitat_count)
#Prepare for plotting
habitat_for_plotting = habitat_poll %>% 
distinct(SafeNet_habitat, Habitat_count)
#Prepare plots
h1 = ggplot(habitat_for_plotting, aes(fct_reorder(SafeNet_habitat,-Habitat_count), Habitat_count)) +
geom_col(fill = "gray20") +
theme(axis.text.x = element_blank()) +
ylab("Number of studies") +
xlab(NULL) +
ylim(0, 30) + 
ggtitle("Habitat type")

h2 = ggplot(habitat_poll, 
aes(fill=Pollinator_order, y=Proportion, x=fct_reorder(SafeNet_habitat,-Habitat_count))) + 
geom_bar(position="stack", stat="identity")+
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL) +
ylab("Proportion of pollinator orders")

h1/h2

#----------------------------
#Calculate MAIN Poll orders by bioregion
#----------------------------
#Now try to explore pollinator orders by habitat 
main_orders = c("Hymenoptera", "Lepidoptera", "Coleoptera", "Diptera")

pollinator_percent_bio = data1 %>%
group_by(Bioregion, Pollinator_order) %>%
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
filter(Pollinator_order %in% main_orders) %>% 
summarise(count = n()) %>%
group_by(Bioregion) %>%
mutate(Proportion = count/sum(count)) %>%
select(-count) 
#Plot orders
bioregion_by_poll =  ggplot(pollinator_percent_bio, 
aes(fill=Pollinator_order, y=Proportion, x=Bioregion)) + 
geom_bar(position="stack", stat="identity")+
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL)
#----------------------------
#Plot by bioregion
#----------------------------
#Left_join
bioregion_poll = left_join(pollinator_percent_bio, bioregion)
#Order dataset
bioregion_poll = bioregion_poll %>% arrange(-Bioregion_count)
#Prepare for plotting
bio_for_plotting = bioregion_poll %>% 
distinct(Bioregion, Bioregion_count)
#Prepare plots
b1 = ggplot(bio_for_plotting, aes(fct_reorder(Bioregion,-Bioregion_count), Bioregion_count)) +
geom_col(fill = "gray20", width = 0.35) +
theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
ylab("Number of studies") +
xlab(NULL) +
ylab(NULL)+
ylim(0, 30) + 
ggtitle("Bioclimatic region")


b2 = ggplot(bioregion_poll, 
aes(fill=Pollinator_order, y=Proportion, x=fct_reorder(Bioregion,-Bioregion_count))) + 
geom_bar(position="stack", stat="identity", width = 0.35)+
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 7),
      axis.text.y = element_blank()) +
xlab(NULL) +
ylab(NULL) +
coord_cartesian(expand = FALSE)

b1/b2
#Plot all together
(h1/h2 | b1/b2) +
plot_layout(guides = 'collect')


#Preparing alternative plot
a1 = habitat %>% 
mutate(Group = "Habitat") %>% 
rename(Habitat = SafeNet_habitat) %>% 
rename(Count = Habitat_count)

b1 = bioregion %>% 
mutate(Group = "Bioregion") %>% 
rename(Habitat = Bioregion) %>% 
rename(Count = Bioregion_count)

#Bind datasets
c1 = bind_rows(a1,b1)

#Ggplot
p1 = ggplot(c1, aes(fct_reorder(Habitat,-Count), Count, fill = Group)) +
geom_col(width = 0.9) +
theme_bw() +
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 11)) +
ylab("Number of studies") +
xlab(NULL) +
ylab(NULL)+
ylim(0, 30) + 
coord_cartesian(expand = FALSE) +
facet_grid(~factor(Group, levels=c("Habitat","Bioregion")), scales = "free_x", space = "free") +
theme(strip.text.x = element_text(size = 12, face = "bold"),
legend.position = "none",strip.background = element_rect(
color="black", fill=NULL, size=0.5, linetype="solid"))+
labs(fill = "")



a2 = pollinator_percent_bio %>% 
mutate(Group = "Bioregion") %>% 
rename(Habitat = Bioregion) 

b2 = pollinator_percent %>% 
mutate(Group = "Habitat") %>% 
rename(Habitat = SafeNet_habitat)

c2 = bind_rows(a2, b2) 


l2 = c2 %>% 
filter(Pollinator_order == "Hymenoptera") %>% 
arrange((Proportion)) %>% 
pull(Habitat)

c2$Habitat = factor(c2$Habitat, levels = l2)

p2 = ggplot(c2, 
aes(fill=Pollinator_order, y=Proportion, x=Habitat)) + 
geom_bar(position = position_stack(reverse = TRUE), stat="identity")+
theme_bw() +
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL) +
coord_cartesian(expand = FALSE) +
theme(strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
color="black", fill=NULL, size=0.5, linetype="solid")) +
scale_fill_manual(values = c("brown2","cyan3","orange2", "darkgreen")) +
facet_grid(~factor(Group, levels=c("Habitat","Bioregion")), scales = "free_x", space = "free") 

p2

p1 +p2


c2$Pollinator_order = factor(c$Pollinator_order, levels = c("Hymenoptera", "Diptera", "Lepidoptera", "Coleoptera"))


p1 = ggplot(c2, 
aes(fill=Pollinator_order, y=Proportion, x=Habitat)) + 
geom_bar(position = position_stack(reverse = TRUE), stat="identity")+
theme_bw() +
theme(legend.position = "bottom",
      plot.margin=unit(c(0,0,0,0), "mm"),
      legend.text = element_text(size=8)) +
xlab(NULL) +
theme(strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
color=NA, fill=NA, size=0.5, linetype="solid")) +
scale_fill_manual(values = c("orange2","cyan3", "darkgreen", "brown2"),
                  name = NULL) +
coord_flip(expand = FALSE) +
ggforce::facet_col(~ factor(Group, levels=c("Habitat","Bioregion")), space = "free", scales = "free_y") +
guides(fill = guide_legend(override.aes = list(size = 1))) 




c1$Habitat = factor(c1$Habitat, levels = l2)


p2 =ggplot(c1, aes(Habitat, Count)) +
geom_col(fill = "black",width = 0.9) +
theme_light() +
theme(plot.margin=unit(c(0,0,0,0), "mm"),
      axis.ticks.y = element_blank(), 
  axis.text.y = element_blank(), 
      strip.text.x = element_text(size=12),
      strip.background = element_rect(
color=NA, fill=NA, size=0.5, linetype="solid")) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(),
panel.border = element_blank(),
axis.line = element_line(color = 'black')) +
xlab(NULL) +
ylab("Number of studies")+
ylim(0, 30) + 
theme(legend.position = "none")+
coord_flip(expand = F) +
ggforce::facet_col(~ factor(Group, levels=c("Habitat","Bioregion")), space = "free", scales = "free_y")


p1 + p2 +plot_layout(widths = c(4,2))



#----------------------------
#Calculate MAIN Poll orders by habitat type
#----------------------------
#Now try to explore pollinator orders by habitat 
main_orders = c("Hymenoptera", "Lepidoptera", "Coleoptera", "Diptera")

plant_percent = data1 %>%
group_by(SafeNet_habitat, Plant_order) %>%
filter(Plant_accepted_name!= "Helianthus annuus") %>% 
#filter(Plant_order %in% main_orders) %>% 
summarise(count = n()) %>%
group_by(SafeNet_habitat) %>%
mutate(Proportion = count/sum(count)) %>%
select(-count) 

levels(factor(plant_percent$Plant_order))

#Plot orders
habitat_by_poll =  ggplot(pollinator_percent, 
aes(fill=Pollinator_order, y=Proportion, x=SafeNet_habitat)) + 
geom_bar(position="stack", stat="identity")+
theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL)
