#Compute vaznull null model from bipartite
#Load libraries
library(dplyr)
library(bipartite)
library(purrr)
library(tidyr)

#Read matrices
matrices = readRDS("Data/Working_files/matrices_by_network.rds")

# Function to perform permatswap and extract permutations
get_permutations <- function(mat) {
  null_model_results <- permatswap(mat, method = "quasiswap", fixedmar = "both", shuffle = "both", times = 100)
  # Extract permutations as matrices
  null_model_results$perm
}

# Apply permatswap and store the results
null_networks <- matrices %>%
  mutate(Null_networks = map(Matrices, get_permutations)) %>%
  select(Network_id, Null_networks) %>%
  unnest_longer(Null_networks, indices_include = FALSE)


#Compute nestedness for the nulls 
null_metrics_networks = null_networks %>% 
mutate(Classic_nestedness = map(Null_networks, ~ nodf_cpp(.))) %>% 
mutate(Connectance = map(Null_networks, ~ networklevel(., index="connectance"))) %>%
select(!Null_networks) %>% 
unnest(cols = c(Classic_nestedness, Connectance)) %>% 
group_by(Network_id) %>% 
summarise(Mean_null_classic_nestedness= mean(Classic_nestedness),
          Deviation_null_classic_nestedness= sd(Classic_nestedness),
          Mean_null_connectance = mean(Connectance),
          Deviation_null_connectance = sd(Connectance))

#Save data
saveRDS(null_networks, "Data/Working_files/null_networks_quasiswap.rds")
saveRDS(null_metrics_networks, "Data/Working_files/null_metrics_networks_quasiswap.rds")

