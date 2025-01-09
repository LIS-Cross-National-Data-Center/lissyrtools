


datasets <- tibble::as_tibble(haven::read_dta("S:/Upload/includefiles/datasets.dta"))
usethis::use_data(datasets, datasets, overwrite = TRUE)
