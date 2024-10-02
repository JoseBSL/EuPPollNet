#Read data
null_metrics_networks = readRDS("Data/Working_files/null_metrics_networks_nullmodel2.rds")
metrics_by_network = readRDS("Data/Working_files/metrics_by_network.rds")

#Safety check
networks_to_check = null_metrics_networks %>% 
filter(Network_id == "1_Bartomeus_BAT1CA_2005")
null_networks = readRDS("Data/Working_files/null_networks_nullmodel2.rds")
null_networks_to_check =  null_networks%>% 
filter(Network_id == "1_Bartomeus_BAT1CA_2005")

metrics_check = null_networks_to_check %>% 
mutate(Classic_nestedness = map(Null_networks, ~ classic_nestedness(.))) %>% 
mutate(Nestedness_temp = map(Null_networks, ~ nestedness_temp(.))) %>% 
select(!Null_networks) %>% 
unnest(cols = c(Classic_nestedness, Nestedness_temp)) %>% 
group_by(Network_id) %>% 
summarise(Mean_null_classic_nestedness= mean(Classic_nestedness),
          Deviation_null_classic_nestedness= sd(Classic_nestedness),
          Mean_null_nestedness_temp = mean(Nestedness_temp),
          Deviation_null_nestedness_temp= sd(Nestedness_temp))
metrics_check == networks_to_check
#Classic nestedness is equal but no nestedness temp
#Check why
nestedness_temp(null_networks_to_check$Null_networks[[1]])
#It always changes, so it is likely ok.

#Finally, find % of networks that differ statistically
# Merge and process data
d = left_join(metrics_by_network, null_metrics_networks, by = "Network_id")
d = d %>% 
mutate(z_score = (Classic_nestedness - Mean_null_classic_nestedness) / Deviation_null_classic_nestedness)

p = 0.05 #cutoff probability 95% confidence
critical_value = qnorm(p/2) #double tail probability divide by 2

d1 = d %>%
mutate(infra_over_represented = case_when(
z_score < -abs(critical_value) ~ "Under-represented",
between(z_score, -abs(critical_value), abs(critical_value)) ~ "No statistical difference",
    z_score > abs(critical_value) ~ "Over-represented"
  ))

#Calculate percentages
colnames(d1)
z_percent = d1 %>% 
group_by(infra_over_represented) %>% 
summarise(percent = length(infra_over_represented)/nrow(.) * 100)

# Plot
p_classic_nestedness = d1 %>% 
  ggplot(aes(x = z_score)) +
  geom_histogram(aes(y = ..count.., fill = infra_over_represented), 
                 bins = 100, alpha = 0.5, color = "black", position = "stack") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  geom_vline(xintercept = -abs(critical_value), linetype = "longdash", colour = "red") +
  geom_vline(xintercept = abs(critical_value), linetype = "longdash", colour = "red") +
  ylab("Density") +
  xlab("Z scores")  +
 # xlim(-6, 9) +
  scale_fill_manual(name = "Observed against null",
                    limits = c("Under-represented", "No statistical difference", "Over-represented"),
                    labels = c("Less nested", "No difference", "More nested"),
                    values = c("Under-represented" = "coral2", 
                               "No statistical difference" = "palegreen3", 
                               "Over-represented" = "cyan3")) +
ggtitle("Classic nestedness")

p_classic_nestedness




