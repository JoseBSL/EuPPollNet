hp.incid <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/hp.incid.txt')
DataInfo(hp.incid, datatype = 'incidence')
out.inc <- iNEXT(hp.incid, datatype = 'incidence_freq')
ggiNEXT(out.inc, type=2) 


class(hp.incid)

data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#Select columns of interest
data1 = data %>% 
mutate(Network_id = paste0(Study_id, Network_id)) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
filter(Plant_rank == "SPECIES") %>% 
mutate(Interaction_id = paste0(Pollinator_accepted_name, "_", Plant_accepted_name)) %>% 
select(Study_id, Pollinator_accepted_name) %>% 
distinct() 


# Count occurrences of each species in each study
result = data1 %>%
count(Study_id, Pollinator_accepted_name) %>% 
group_by(Pollinator_accepted_name) %>% 
summarise(Incidence = sum(n)) 
pull(Incidence)

d = matrix(c(length(unique(data1$Study_id)) , 
          result %>% pull(Incidence)),  
          ncol = 1)
row.names(d) = c("Plot", result %>% pull(Pollinator_accepted_name))
d = data.frame(d)
colnames(d) = "Studies"
out.inc <- iNEXT(d, datatype = 'incidence_freq')

ggiNEXT(out.inc, type=1) 


