# Load libraries
library(dplyr)
library(bipartite)
library(purrr)
library(tidyr)
library(maxnodf)  # Ensure this library is included
# Ensure vegan is loaded as well
library(vegan)

# Read matrices
matrices = readRDS("Data/Working_files/matrices_by_network.rds")

# Custom function to generate null matrices
cell_model <- function(d, t_max) {
  
  rows <- nrow(d)
  columns <- ncol(d)
  null_matrices <- list()  # Initialize an empty list to store null matrices
  
  # Run the loop exactly t_max times
  for (t in 1:t_max) {  

    PR <- numeric(rows)  # Initialize a vector for row probabilities
    PC <- numeric(columns)  # Initialize a vector for column probabilities
    B <- matrix(0, rows, columns)  # Initialize the resulting null matrix
    
    # Calculate probabilities for rows (PR)
    for (i in 1:rows) {
      number_ones <- sum(d[i, ] == 1)  # Count number of ones in row i
      PR[i] <- number_ones / columns
    }
    
    # Calculate probabilities for columns (PC)
    for (j in 1:columns) {
      number_ones <- sum(d[, j] == 1)  # Count number of ones in column j
      PC[j] <- number_ones / rows
    }
    
    # Generate the matrix B based on probabilities
    for (i in 1:rows) {
      for (j in 1:columns) {
        p <- (PR[i] + PC[j]) / 2
        r <- runif(1) 
        if (r < p) {  
          B[i, j] <- 1
        }
      }
    }

    null_matrices[[t]] <- B  # Store the generated matrix B in the list at index t
  }
  
  return(null_matrices)  # Return the list of null matrices
}

# Create null networks with standardized matrices using the custom cell_model function
null_networks = matrices %>%
  mutate(Binary_Matrices = map(Matrices, ~ ifelse(. > 0, 1, 0))) %>%  # Convert each matrix to binary
  mutate(Null_networks = map(Binary_Matrices, ~ cell_model(., 100))) %>% 
  select(Network_id, Null_networks) %>% 
  unnest(cols = c(Null_networks))  # This will create a long table of models

classic_nestedness = function(interaction_matrix){
# Calculate nestedness using the nestedtemp function
s = nestednodf(interaction_matrix)
# Return the statistic
return(s$statistic[3]/100)
}
# Define the nestedness function
nestedness_temp = function(interaction_matrix) {
# Calculate nestedness using the nestedtemp function
s = nestedtemp(interaction_matrix)
# Return the statistic
return(s$statistic/100)
}
#Compute nestedness for the nulls 
null_metrics_networks = null_networks %>% 
mutate(Classic_nestedness = map(Null_networks, ~ classic_nestedness(.))) %>% 
mutate(Nestedness_temp = map(Null_networks, ~ nestedness_temp(.))) %>% 
select(!Null_networks) %>% 
unnest(cols = c(Classic_nestedness, Nestedness_temp)) %>% 
group_by(Network_id) %>% 
summarise(Mean_null_classic_nestedness= mean(Classic_nestedness),
          Deviation_null_classic_nestedness= sd(Classic_nestedness),
          Mean_null_nestedness_temp = mean(Nestedness_temp),
          Deviation_null_nestedness_temp= sd(Nestedness_temp))

saveRDS(null_networks, "Data/Working_files/null_networks_nullmodel2.rds")
saveRDS(null_metrics_networks, "Data/Working_files/null_metrics_networks_nullmodel2.rds")

        
      