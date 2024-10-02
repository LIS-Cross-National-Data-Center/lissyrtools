# technical_variables.R
# write vectors with names of technical variables

names_technical_variables_pfile <- c("hid", "pid", "did", "dname", "cname", "iso2", "iso3", "year",
                               "wave", "ppopwgt", "pwgt", "pwgta", "currency", "grossnet")

names_technical_variables_hfile <- c("hid", "did", "dname", "cname", "iso2", "iso3", "year", "wave",
                               "hpopwgt", "hwgt", "hwgta", "currency", "grossnet")


usethis::use_data(names_technical_variables_pfile, internal = FALSE, overwrite = TRUE)
usethis::use_data(names_technical_variables_hfile, internal = FALSE, overwrite = TRUE)


