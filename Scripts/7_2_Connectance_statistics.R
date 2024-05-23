#Explore how connectance is associated with the geometric mean of species
#Explore how the residuals from this association, how much connectance
#deviates from their expected association given an x number of species
#is associated with latitude
#Graph 1 and 2 that will be (A) and (C) in the paper

#Calculate residual connectance (similar to Olesen and Jordano 2002)

#Load libraries
library(dplyr)
library(ggplot2)

#Read general data
data = readRDS("Data/3_Final_data/Interaction_data.rds")

# Subset data for studies with only bumblebees
v = c("13_Karise", "22_Kallnik")
subsetting = unique(data$Study_id)

#Prepare dates
data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))  %>% 
filter(!Study_id %in% v) %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id, "_", Year)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) 

#Prepare coordinates
long_format_coords = data %>% 
select(Network_id, Latitude, Longitude, Bioregion) %>% 
distinct()
#Read network metric data
metrics = readRDS("Data/Working_files/metrics_by_network.rds")

#Bind coordinates
d_metrics = left_join(metrics, long_format_coords)

#Explore distribution of connectance
d_metrics %>% 
ggplot(aes(Connectance)) +
geom_histogram()

#Calculate geometric mean of species
library(psych)
species_number = data %>% 
group_by(Network_id) %>% 
summarise(N_plants = n_distinct(Plant_accepted_name),
          N_pollinators = n_distinct(Pollinator_accepted_name)) %>% 
group_by(Network_id) %>% 
summarise(geometric_mean_spp = geometric.mean(c(N_plants, N_pollinators)))

#Add it to network metric dataset
metrics = left_join(d_metrics, species_number)

#Explore their association
p1 = metrics %>% 
ggplot(aes(geometric_mean_spp, Connectance)) +
geom_point(pch = 21, fill = "azure3") +
theme_bw() +
xlab("Species")+
ggtitle("(a)")
p1

metrics$log_geometric_mean_spp = log(metrics$geometric_mean_spp)
#install.packages("betareg")
library(betareg)
model = betareg(Connectance ~ log_geometric_mean_spp, data = metrics)
summary(model)

saveRDS(metrics, "Data/Manuscript_info/Figure6_connectance_species.rds")
p1 = ggplot(metrics, aes(x = log_geometric_mean_spp, y = Connectance))+
geom_point(pch = 21, fill = "azure3") +
geom_line(aes(y = predict(model, metrics)), colour= "black")+
xlab("log(Species)") +
theme_bw()
p1
#Extract residuals
metrics$residual_conectance = residuals(model, type = "response")
#Check distribution
ggplot(metrics, aes(x=residual_conectance)) +
geom_histogram()
#Looks ok for lm
residual_conectance_latitude = lm(residual_conectance ~ Latitude , data = metrics)
summary(residual_conectance_latitude)
glance(residual_conectance_latitude)
#Get a nice summary from broom package
connectance_lat = glance(residual_conectance_latitude)
#Save values to load them in r markdown
connectance_lat_r2 = connectance_lat$adj.r.squared
connectance_lat_pval = connectance_lat$p.value[[1]]
#Save values
saveRDS(connectance_lat_r2, "Data/Manuscript_info/connectance_lat_r2.rds")
saveRDS(connectance_lat_pval, "Data/Manuscript_info/connectance_lat_pval.rds")

#Predict values with confidence intervals
prediction_intervals <- predict(
  residual_conectance_latitude, 
  newdata = d_metrics, 
  interval = "confidence")

#Add to dataset
metrics = cbind(metrics, prediction_intervals)
#Plot (provisional)
ggplot(data = metrics, aes(x=Latitude, 
       y=residual_conectance,
       color=Bioregion)) +
geom_point() +
geom_line(data = metrics,aes(x=Latitude, y = fit), inherit.aes = FALSE)+
geom_ribbon(data = metrics, 
    aes(x=Latitude,ymin = lwr, ymax = upr), inherit.aes = FALSE,
    fill = "grey70", alpha= 0.3) +
theme_bw()
saveRDS(metrics, "Data/Manuscript_info/residual_connectance.rds")
saveRDS(metrics, "Data/Manuscript_info/Figure6_connectance_latitude.rds")
#Final plot
p2 = ggplot(data = metrics, aes(x=Latitude, 
       y=residual_conectance,
       color=Bioregion)) +
geom_point(aes(fill = Bioregion, shape = Bioregion), stroke = 0.25, size=2) +
geom_ribbon(data = metrics, 
    aes(x=Latitude,ymin = lwr, ymax = upr), inherit.aes = FALSE,
    fill = "grey70", alpha= 0.3) +
geom_line(data = metrics,aes(x=Latitude, y = fit), inherit.aes = FALSE)+
scale_fill_viridis_d() +
scale_colour_viridis_d() +
scale_shape_manual(values = c(
    "Alpine" = 21,  # Circle
    "Atlantic" = 22, # Triangle
    "Boreal" = 23,   # Square
    "Continental" = 24,  # Diamond
    "Mediterranean" = 25  # Cross
  ))+
ylab("Residual connectance") +
ggtitle("(c)") +
theme_bw() 
p2

library(patchwork)
p1 + p2

#Calculate correlation between metrics and species mean
##Connectance
connectance_geometric_mean_corr = 
   cor.test(metrics$geometric_mean_spp, 
   metrics$Connectance,
   method = "kendall")
#Prepare outputs to be saved
connectance_spp_mean_corr_tau = connectance_geometric_mean_corr$estimate[[1]]
connectance_spp_mean_corr_pval = connectance_geometric_mean_corr$p.value

#Save
saveRDS(connectance_spp_mean_corr_tau, "Data/Manuscript_info/connectance_spp_mean_corr_tau.rds")
saveRDS(connectance_spp_mean_corr_pval, "Data/Manuscript_info/connectance_spp_mean_corr_pval.rds")

#Expore statististical differences of connectance across bioclimatic regions
connectance_bioregion = lm(Connectance ~ Bioregion, data = metrics)
marginal = emmeans(connectance_bioregion, "Bioregion")
pairs(marginal)

#Find min, max values of connectance
min_connectance = metrics %>%  
slice_min(Connectance) %>% 
pull(Connectance)
#
max_connectance = metrics %>%  
slice_max(Connectance) %>% 
pull(Connectance)
#
mean_connectance = mean(metrics$Connectance)
#Save values
saveRDS(min_connectance[[1]], "Data/Manuscript_info/min_connectance.rds")
saveRDS(max_connectance[[1]], "Data/Manuscript_info/max_connectance.rds")
saveRDS(mean_connectance[[1]], "Data/Manuscript_info/mean_connectance.rds")


#Quick check
resid_connectance_nestedness = 
cor.test(metrics$residual_conectance, 
metrics$Normalised_nestedness,
method = "kendall")



