


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

#select unique cases from one of the lists
single_cases <- bind_rows(lapply(list1, function(x) x %>% select(col2) %>% distinct(col2)))

#How to select unique cases from the list of lists. 

