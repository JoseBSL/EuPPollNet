#Read data
null_metrics_networks = readRDS("Data/Working_files/null_metrics_networks_nullmodel2.rds")
metrics_by_network = readRDS("Data/Working_files/metrics_by_network.rds")

#Finally, find % of networks that differ statistically
# Merge and process data
d = left_join(metrics_by_network, null_metrics_networks, by = "Network_id")
d = d %>% 
mutate(z_score = (Nestedness_temp - Mean_null_nestedness_temp) / Deviation_null_nestedness_temp)

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
p_nestedness_temp = d1 %>% 
  ggplot(aes(x = z_score)) +
  geom_histogram(aes(y = ..count.., fill = infra_over_represented), 
                 bins = 100, alpha = 0.5, color = "black", position = "stack") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  geom_vline(xintercept = -abs(critical_value), linetype = "longdash", colour = "red") +
  geom_vline(xintercept = abs(critical_value), linetype = "longdash", colour = "red") +
  ylab("Number of networks") +
  xlab("Z scores")  +
 # xlim(-6, 9) +
  scale_fill_manual(name = "Observed against null",
                    limits = c("Under-represented", "No statistical difference", "Over-represented"),
                    labels = c("Less nested", "No difference", "More nested"),
                    values = c("Under-represented" = "coral2", 
                               "No statistical difference" = "palegreen3", 
                               "Over-represented" = "cyan3")) +
ggtitle("Nestedness temp")

p_nestedness_temp


