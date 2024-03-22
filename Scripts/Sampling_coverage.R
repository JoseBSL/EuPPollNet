#Read data
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")
#Select columns of interest
data1 = data %>% 
mutate(Network_id = paste0(Study_id, Network_id)) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
filter(Plant_rank == "SPECIES") %>% 
mutate(Interaction_id = paste0(Pollinator_accepted_name, "_", Plant_accepted_name)) %>% 
select(Study_id, Pollinator_accepted_name) %>% 
distinct() 

# Create a presence-absence matrix
presence_matrix <- table(data1$Study_id, data1$Pollinator_accepted_name)

# Check the class of presence_matrix
print(class(presence_matrix))

# Convert the presence-absence matrix to a matrix and ensure numeric entries
presence_matrix <- as.matrix(presence_matrix)
presence_matrix <- as.numeric(presence_matrix > 0)

presence_matrix = c(length(unique(data1$Study_id)), presence_matrix)

# Calculate sampling coverage using iNEXT
out <- iNEXT(presence_matrix, datatype = "incidence_freq", endpoint = 2)

sampling_coverage$DataInfo

p1 <- ggiNEXT(out, type=1)+ theme_classic() +   #  type 1 = the diversity estimator
        labs(x = "Survey sites", y = "Richness")


