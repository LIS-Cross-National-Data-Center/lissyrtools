# create_ppp_files
deflators <- tibble::as_tibble(read.delim("I:/ppp_2017.txt", sep = ","))


usethis::use_data(deflators, deflators, overwrite = TRUE)
