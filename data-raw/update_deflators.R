


# create_ppp_files
deflators <- tibble::as_tibble(read.delim("/media/includefiles/ppp_2017.txt", sep = ","))
usethis::use_data(deflators, deflators, overwrite = TRUE)

