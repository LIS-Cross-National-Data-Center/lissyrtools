


# create_ppp_files

if(version$os ==  "linux-gnu"){
  deflators <- tibble::as_tibble(read.delim("/media/includefiles/ppp_2017.txt", sep = ","))
  usethis::use_data(deflators, deflators, overwrite = TRUE)

} else {
  deflators <- tibble::as_tibble(read.delim("S:/Upload/includefiles/ppp_2017.txt", sep = ","))
  usethis::use_data(deflators, deflators, overwrite = TRUE)
}
