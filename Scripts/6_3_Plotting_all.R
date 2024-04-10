
library(dplyr)
library(ggplot2)
library(scales)


#Pollinators-----
poll_output = readRDS("Data/Working_files/pollinator_sampling_coverage.rds")
pollinators_curves = readRDS("Data/Working_files/collectors_curves_pollinators.rds")
#Create dataset
d_poll = poll_output$iNextEst$Network_id
#Check cols
colnames(d_poll)
d_poll$col = d_poll$method

#Now create tibble with same structure
a_poll = tibble(t = pollinators_curves$Study_sampled, 
           method = as.character(pollinators_curves$iteration), 
           qD = pollinators_curves$Unique_spp_cumulative,
           col = "collectors")

#Bind datasets 
d_poll1 = bind_rows(d_poll, a_poll)
d_poll1_point = d_poll1 %>%  filter(method == "observed")
d_poll1_lines = d_poll1 %>%  filter(!method == "observed")

#Order levels
d_poll1_lines$col = factor(d_poll1_lines$col, levels = c("collectors", "interpolated", "extrapolated"))
#Plot
p1 = ggplot(d_poll1_lines, aes(x = t, y = qD, group = method, color = col)) +
geom_line(aes(linetype = col)) +
scale_colour_manual(values = c("gray", "black", "red"), labels = c("Randomizations", "Interpolated", "Extrapolated")) +
scale_linetype_manual(values = c("solid", "solid", "dashed"),  labels = c("Randomizations", "Interpolated", "Extrapolated")) + 
geom_ribbon(aes(ymin = qD.LCL, 
    ymax=qD.UCL, fill = col),alpha=0.2, colour = NA, show.legend = FALSE)+
scale_fill_manual(values = c("gray", "black", "red")) +
geom_point(data = d_poll1_point, col = "black") + 
ylab("Pollinator species") +
xlab("Sampling sites") +
theme_bw() +
coord_cartesian(expand = FALSE)  +
xlim(0,3000)+
ylim(0, 3000) +
ggtitle("(a)")

#Plants-----
plant_output = readRDS("Data/Working_files/plant_sampling_coverage.rds")
plant_curves = readRDS("Data/Working_files/collectors_curves_plants.rds")
#Create dataset
d_plant = plant_output$iNextEst$Network_id
#Check cols
colnames(d_plant)
d_plant$col = d_plant$method

#Now create tibble with same structure
a_plant = tibble(t = plant_curves$Study_sampled, 
           method = as.character(plant_curves$iteration), 
           qD = plant_curves$Unique_spp_cumulative,
           col = "collectors")

#Bind datasets 
d_plant1 = bind_rows(d_plant, a_plant)
d_plant1_point = d_plant1 %>%  filter(method == "observed")
d_plant1_lines = d_plant1 %>%  filter(!method == "observed")

#Order levels
d_plant1_lines$col = factor(d_plant1_lines$col, levels = c("collectors", "interpolated", "extrapolated"))
#Plot
p2 = ggplot(d_plant1_lines, aes(x = t, y = qD, group = method, color = col)) +
geom_line(aes(linetype = col)) +
scale_colour_manual(values = c("gray", "black", "red"), labels = c("Randomizations", "Interpolated", "Extrapolated")) +
scale_linetype_manual(values = c("solid", "solid", "dashed"),  labels = c("Randomizations", "Interpolated", "Extrapolated")) + 
geom_ribbon(aes(ymin = qD.LCL, 
    ymax=qD.UCL, fill = col),alpha=0.2, colour = NA, show.legend = FALSE)+
scale_fill_manual(values = c("gray", "black", "red")) +
geom_point(data = d_plant1_point, col = "black") + 
ylab("Plant species") +
xlab("Sampling sites") +
theme_bw() +
coord_cartesian(expand = FALSE)  +
xlim(0,3000)+
ylim(0, 3000)+
ggtitle("(b)")



#Interactions-----
int_output = readRDS("Data/Working_files/interactions_sampling_coverage.rds")
interactions_curves = readRDS("Data/Working_files/collectors_curves_interactions.rds")
#Create dataset
d_int = int_output$iNextEst$Network_id
#Check cols
colnames(d_int)
d_int$col = d_int$method

#Now create tibble with same structure
a_poll = tibble(t = interactions_curves$Study_sampled, 
           method = as.character(interactions_curves$iteration), 
           qD = interactions_curves$Unique_spp_cumulative,
           col = "collectors")

#Bind datasets 
d_int1 = bind_rows(d_int, a_poll)
d_int1_point = d_int1 %>%  filter(method == "observed")
d_int1_lines = d_int1 %>%  filter(!method == "observed")

#Plot
#Order levels
d_int1_lines$col = factor(d_int1_lines$col, levels = c("collectors", "interpolated", "extrapolated"))
#Plot
p3 = ggplot(d_int1_lines, aes(x = t, y = qD, group = method, color = col)) +
geom_line(aes(linetype = col)) +
scale_colour_manual(values = c("gray", "black", "red"), labels = c("Randomizations", "Interpolated", "Extrapolated")) +
scale_linetype_manual(values = c("solid", "solid", "dashed"),  labels = c("Randomizations", "Interpolated", "Extrapolated")) + 
geom_ribbon(aes(ymin = qD.LCL, 
    ymax=qD.UCL, fill = col),alpha=0.2, colour = NA, show.legend = FALSE)+
scale_fill_manual(values = c("gray", "black", "red")) +
geom_point(data = d_int1_point, col = "black") + 
ylab("Interactions") +
xlab("Sampling sites") +
theme_bw() +
coord_cartesian(expand = FALSE)  +
xlim(0,3000) +
ylim(0, 30000)+
ggtitle("(c)")

#Combine plots 
library(patchwork)
p1 + p2 + p3 + plot_layout(guides = "collect") &   theme(legend.title=element_blank())



#----------------#
#Pollinators
#----------------#
#Read data
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")%>% 
mutate(Network_id = paste0(Study_id, Network_id))  
#First select cols of rank, accepted name and study ID
#Then select unique levels by study ID
#Now sum levels (maximum can be as maximum number of studies)
poll_spread = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Network_id) %>% 
group_by(Network_id) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
distinct(Pollinator_accepted_name) %>% 
ungroup() %>% 
count(Pollinator_accepted_name) %>% 
rename(n_networks_shared = n) %>% 
arrange(-n_networks_shared) %>% 
mutate(Percent_total = n_networks_shared / n_distinct(data$Network_id))
  
#Seelect quantile 50% from the total number of species
spp_number = poll_spread %>%  
select(Pollinator_accepted_name) %>% 
n_distinct()
df = seq(1, spp_number)
quantile_50 = quantile(df, 0.5)

#Obtain most common pollinator for plotting
most_common_poll = poll_spread %>% filter(n_networks_shared == max(n_networks_shared)) %>%
select(Pollinator_accepted_name) %>% pull() 

#Plot
o1 = ggplot(poll_spread, 
aes(reorder(Pollinator_accepted_name,-Percent_total), Percent_total)) +
geom_bar(stat = "identity", 
           color = "#D55E00",
           lwd = 0.015, fill = "#D55E00") +
ylab("Shared pollinators (%)") +
theme_minimal()+
theme(axis.text.x = element_blank(),
  axis.ticks.x=element_blank(),
  axis.ticks.y = element_line(size = 0.2),
  axis.text.y = element_text(size=5),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_line(size=0.2, linetype = 2)) +
xlab("Pollinator species") +
ylab("Occurrence across sampling sites") + 
geom_vline(xintercept = quantile_50, linetype="dashed", 
color = "black", size=0.25) + 
annotate(geom="text", x=1270, y=0.60, label="50th percentile",color="black", size = 1.5) +
annotate(geom="text", x=20, y=0.66, label=most_common_poll[1],color="black",  fontface = 'italic', size = 1.5)+
annotate(geom="text", x=20, y=-0.02, label="Common spp."
      ,color="black",  size = 1.5) +
annotate(geom="text", x=1800, y=-0.02, label="\'Rare\' spp."
      ,color="black",  size = 1.5)  +
scale_y_continuous(breaks = seq(0, 0.7, by= 0.25), expand = c(0, 0)) + 
coord_cartesian(xlim=c(0, nlevels(factor(poll_spread$Pollinator_accepted_name))),
                ylim=c(0,0.7), clip="off") +
ggtitle("(d)")

#----------------#
#Plants
#----------------#
plant_spread = data %>% 
select(Plant_rank, Network_id, Plant_accepted_name) %>% 
group_by(Network_id) %>% 
filter(Plant_rank == "SPECIES") %>% 
distinct(Plant_accepted_name) %>% 
ungroup() %>% 
count(Plant_accepted_name) %>% 
rename(n_networks_shared = n) %>% 
arrange(-n_networks_shared) %>% 
mutate(Percent_total = n_networks_shared / n_distinct(data$Network_id))
  
#Seelect quantile 50% from the total number of species
spp_number = plant_spread %>%  
select(Plant_accepted_name) %>% 
n_distinct()
df = seq(1, spp_number)
quantile_50 = quantile(df, 0.5)

#Obtain most common plant for plotting
most_common_plant = plant_spread %>% filter(n_networks_shared == max(n_networks_shared)) %>%
select(Plant_accepted_name) %>% pull() 

o2 = ggplot(plant_spread, 
aes(reorder(Plant_accepted_name,-Percent_total), Percent_total, fill = Plant_accepted_name)) +
geom_bar(stat = "identity", 
           color = "#009E73",
           lwd = 0.015, fill = "#009E73") +
theme_minimal()+
theme(axis.text.x = element_blank(),
         axis.ticks.x=element_blank(),
         axis.ticks.y = element_line(size = 0.2),
       #  axis.title.x = element_text(face = "bold", size = 7), plot.title = element_text(size = 8, face = "bold"),
         axis.text.y = element_text(size=5),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_line(size=0.2, linetype =  2)) +
xlab("Plant species") +
ylab("Occurrence across sampling sites") + 
geom_vline(xintercept = quantile_50, linetype="dashed", 
          color = "black", size=0.25) + 
annotate(geom="text", x=835, y=0.60, label="50th percentile",color="black", size = 1.5) + 
annotate(geom="text", x=20, y=0.4, label=most_common_plant[1]
      ,color="black",  fontface = 'italic', size = 1.5)+
annotate(geom="text", x=20, y=-0.02, label="Common spp."
      ,color="black",  size = 1.5) +
annotate(geom="text", x=1200, y=-0.02, label="\'Rare\' spp."
      ,color="black",  size = 1.5) + 
scale_y_continuous(breaks = seq(0, 0.7, by= 0.25), expand = c(0, 0), labels = label_number(accuracy = 0.01)) +
coord_cartesian(xlim=c(0, nlevels(factor(plant_spread$Plant_accepted_name))),
                ylim=c(0, 0.7), clip="off") +
ggtitle("(e)")

#Pollinator by plant species
plant_poll_curves = readRDS("Data/Working_files/collectors_curves_plant_ploll.rds")
plant_poll_output = readRDS("Data/Working_files/plant_poll_spp_sampling_coverage.rds")

#Create dataset
d_pp = plant_poll_output$iNextEst$Plant_id
#Check cols
colnames(d_pp)
d_pp$col = d_pp$method

#Now create tibble with same structure
a_poll = tibble(t = plant_poll_curves$Plant_sampled, 
           method = as.character(plant_poll_curves$iteration), 
           qD = plant_poll_curves$Unique_spp_cumulative,
           col = "collectors")

#Bind datasets 
d_pp1 = bind_rows(d_pp, a_poll)
d_pp1_point = d_pp1 %>%  filter(method == "observed")
d_pp1_lines = d_pp1 %>%  filter(!method == "observed")

#Plot
#Order levels
d_pp1_lines$col = factor(d_pp1_lines$col, levels = c("collectors", "interpolated", "extrapolated"))
#Plot
pp1 = ggplot(d_pp1_lines, aes(x = t, y = qD, group = method, color = col)) +
geom_line(aes(linetype = col)) +
scale_colour_manual(values = c("gray", "black", "red"), labels = c("Randomizations", "Interpolated", "Extrapolated")) +
scale_linetype_manual(values = c("solid", "solid", "dashed"),  labels = c("Randomizations", "Interpolated", "Extrapolated")) + 
geom_ribbon(aes(ymin = qD.LCL, 
    ymax=qD.UCL, fill = col),alpha=0.2, colour = NA, show.legend = FALSE)+
scale_fill_manual(values = c("gray", "black", "red")) +
geom_point(data = d_pp1_point, col = "black") + 
ylab("Pollinator species") +
xlab("Plant species") +
theme_bw() +
coord_cartesian(expand = FALSE) +
ggtitle("(f)")                          


#Plot all
a = p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.title=element_blank())
b = o1 + o2 + pp1

a/b



