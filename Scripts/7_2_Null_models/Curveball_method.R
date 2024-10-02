#Compute vaznull null model from bipartite
#Load libraries
library(dplyr)
library(bipartite)
library(purrr)
library(tidyr)

#Read matrices
matrices = readRDS("Data/Working_files/matrices_by_network.rds")

# Function to simulate 10 null matrices and store them as a list
simulate_null_matrices <- function(binary_matrix) {
  null_model <- vegan::nullmodel(binary_matrix, method = "curveball")
  simulated_array <- simulate(null_model, nsim = 100)
  
  # Convert the 3D array to a list of matrices
  list_of_matrices <- lapply(1:dim(simulated_array)[3], function(i) {
    simulated_array[,,i]
  })
  
  return(list_of_matrices)
}

# Create null networks with standardized matrices
null_networks = matrices %>%
  mutate(Binary_Matrices = map(Matrices, ~ ifelse(. > 0, 1, 0))) %>%  # Convert each matrix to binary
  mutate(Null_networks = map(Binary_Matrices, simulate_null_matrices)) %>% 
  select(Network_id, Null_networks)%>%
  unnest(Null_networks)


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
saveRDS(null_networks, "Data/Working_files/null_networks_curveball.rds")
saveRDS(null_metrics_networks, "Data/Working_files/null_metrics_networks_curveball.rds")

