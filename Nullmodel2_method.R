#Compute vaznull null model from bipartite
#Load libraries
library(dplyr)
library(bipartite)
library(purrr)
library(tidyr)
library(maxnodf)
#Read matrices
matrices = readRDS("Data/Working_files/matrices_by_network.rds")

# Function to create Null Model 2 based on average probabilities
null_model_2 <- function(interaction_matrix) {
  # Convert the interaction matrix to a binary format
  interaction_matrix_binary <- interaction_matrix > 0
  
  # Calculate row sums and column sums
  row_sums <- rowSums(interaction_matrix_binary)
  col_sums <- colSums(interaction_matrix_binary)
  
  # Calculate the probabilities
  p_row <- row_sums / sum(row_sums)            # Probabilities for rows
  p_col <- col_sums / sum(col_sums)            # Probabilities for columns
  
  # Average the probabilities
  p_matrix <- outer(p_row, p_col, FUN = function(x, y) (x + y) / 2)
  
  # Create the null model matrix
  null_matrix <- matrix(rbinom(length(p_matrix), size = 1, prob = p_matrix), 
                         nrow = nrow(interaction_matrix), 
                         ncol = ncol(interaction_matrix))
  
  return(null_matrix)
}

# Create null networks with standardized matrices
generate_null_models <- function(binary_matrix, n_models) {
  # Generate multiple null models using replicate
  replicate(n_models, null_model_2(binary_matrix), simplify = FALSE)
}

# Set the number of null models to generate
n_null_models <- 100

# Create null models for each binary matrix in the dataset
null_networks = matrices %>%
mutate(Binary_Matrices = map(Matrices, ~ ifelse(. > 0, 1, 0))) %>%  # Convert each matrix to binary
mutate(Null_networks = map(Binary_Matrices, ~ generate_null_models(., n_null_models))) %>% 
select(Network_id, Null_networks) %>% 
unnest(cols = c(Null_networks))  # This will create a long table of models

#Compute nestedness for the nulls 
null_metrics_networks = null_networks %>% 
mutate(Classic_nestedness = map(Null_networks, ~ nodf_cpp(.))) %>% 
#mutate(Connectance = map(Null_networks, ~ networklevel(., index="connectance"))) %>%
select(!Null_networks) %>% 
unnest(cols = c(Classic_nestedness)) %>% 
group_by(Network_id) %>% 
summarise(Mean_null_classic_nestedness= mean(Classic_nestedness),
          Deviation_null_classic_nestedness= sd(Classic_nestedness))

#Save data
saveRDS(null_networks, "Data/Working_files/null_networks_null_model2.rds")
saveRDS(null_metrics_networks, "Data/Working_files/null_metrics_networks_null_model2.rds")


