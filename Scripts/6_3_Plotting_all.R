
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
ylim(0, 3000)

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
ylim(0, 3000)



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
ylim(0, 30000)

#Combine plots 
library(patchwork)
p1 + p2 + p3 + plot_layout(guides = "collect") &   theme(legend.title=element_blank())

