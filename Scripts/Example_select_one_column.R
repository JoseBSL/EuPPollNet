
library(tidyverse)

#Generate datasets to create example
dat1 <- tibble(col1 = runif(10), col2 = stringi::stri_rand_strings(10, 5))
dat2 <- tibble(col1 = runif(10), col2 = stringi::stri_rand_strings(10, 5))
dat3 <- tibble(col1 = runif(10), col2 = stringi::stri_rand_strings(10, 5))
dat4 <- tibble(col1 = runif(10), col2 = stringi::stri_rand_strings(10, 5))

#Generate lists (3 of them for example)
list1 <- list(id1 = dat1, id2 = dat2)
list2 <- list(id1 = dat3)
list3 <- list(id1 = dat4)
#Generate list of lists
final_list <- list(dataset1 = list1, dataset2 = list2, dataset3 = list3)


#select unique cases from the list of lists
data <- NULL

for (i in final_list) {

single_cases <- bind_rows(lapply(i, function(x) x %>% select(col2) %>% distinct(col2)))

data <- rbind(data, single_cases)  
  
}


#select unique cases from one of the lists
single_cases <- bind_rows(lapply(list1, function(x) x %>% select(col2) %>% distinct(col2)))

str(data1)

data1 <- c(list1, list2, list3) %>%
  setNames(paste0("id", seq_along(.))) %>%
  bind_rows(.id = "id")  %>%
  distinct(id, col2) %>%
  select(-id)
