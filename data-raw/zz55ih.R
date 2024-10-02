# zz55ih.R

#' Mock dataset with household-level LIS format.
#'
#' Contains 5 rows of artificial data, with value labels and same attributes
#'   as a LIS file. The attributes are similar to those in the LIS it14ih sample
#'   file from the self-teaching datasets
#'   https://www.lisdatacenter.org/resources/self-teaching/

library(here)

# read the label.table attribute from another script
label.table_attribute <- source(here("data-raw", "label_table_zz55ih.R"))

zz55ih <- structure(list(hid = c(1L, 2L, 3L, 4L, 5L),
                         dhi = c(80000, 23000, 31000, 70000, 29000),
                         rural = structure(c(1L, 1L, 1L, 2L, 2L),
                                           .Label = c("[0]not rural area",
                                                      "[1]rural area"),
                                           class = "factor"),
                         own = structure(c(1L,
                                           2L, 1L, 2L, 1L),
                                         .Label = c("[100]owned", "[110]owned outright",
                                                    "[120]owned with mortgage", "[200]not owned", "[210]rented",
                                                    "[211]rented at market price", "[212]subsidised rent", "[220]free housing",
                                                    "[221]employer provided housing", "[222]government/public provided housing",
                                                    "[223]provided by others", "[224]illegal occupation"), class = "factor"),
                         did = c(555L, 555L, 555L, 555L, 555L),
                         dname = c("zz55",
                                   "zz55", "zz55", "zz55", "zz55"),
                         cname = c("ZZ", "ZZ",
                                   "ZZ", "ZZ", "ZZ"),
                         iso2 = c("zz", "zz", "zz", "zz",
                                  "zz"),
                         iso3 = c("zzz", "zzz", "zzz", "zzz", "zzz"),
                         year = c(5555L, 5555L, 5555L, 5555L, 5555L),
                         wave = structure(c(10L, 10L,
                                            10L, 10L, 10L),
                                          .Label = c("[0]Historical Wave", "[1]Wave I",
                                                     "[2]Wave II", "[3]Wave III", "[4]Wave IV", "[5]Wave V", "[6]Wave VI",
                                                     "[7]Wave VII", "[8]Wave VIII", "[9]Wave IX", "[10]Wave X",
                                                     "[11]Wave XI", "[12]Wave XII", "[13]Wave XIII"),
                                          class = "factor"),
                         grossnet = structure(c(4L, 4L, 4L, 4L, 4L),
                                              .Label = c("[100]gross, taxes and contributions fully captured",
                                                         "[110]gross, taxes and contributions collected", "[120]gross, taxes and contributions imputed",
                                                         "[200]net, taxes and contributions not captured", "[300]mixed, taxes and contributions insufficiently captured",
                                                         "[310]mixed, total income account for full taxes and contributions, subcomponents do not",
                                                         "[320]mixed, total income does not account for full taxes and contributions"),
                                              class = "factor"),
                         hwgt = c(1, 1.2,
                                  0.8, 1.4, 0.6),
                         hpopwgt = c(1000, 1200, 800,
                                     1400, 600),
                         hwgta = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_)),
                    row.names = c(NA, -5L),
                    datalabel = "zz55: version created on 31 Jan 2021 00:00",
                    time.stamp = "31 Jan 2021 00:00",
                    formats = c("%8.0g",
                                "%16.0f", "%17.0g", "%39.0g", "%8.0g", "%9s", "%9s", "%9s", "%9s",
                                "%8.0g", "%18.0g", "%87.0g", "%10.0g", "%10.0g", "%8.0g", "%23.0g"),
                    types = c(65529L, 65526L, 65530L, 65529L, 65529L, 4L, 5L,
                              2L, 3L, 65529L, 65530L, 65529L, 65526L, 65526L, 65530L, 65530L),
                    val.labels = c("", "", rural = "rural", own = "own", "", "",
                                   "", "", "", "", wave = "wave", grossnet = "grossnet", "", "",
                                   ""),
                    var.labels = c("household identifier",
                                   "disposable household income, household", "rural area (dummy)",
                                   "owned/rented housing", "unique country/year number", "country/year identifier",
                                   "country name", "2-letter country abbreviation", "3-letter country abbreviation",
                                   "reference year", "data wave", "gross/net income information",
                                   "normalised household weight", "household weight", "additional household weight",
                                   "household income imputation (dummy)"),
                    version = 118L,
                    byteorder = "LSF",
                    orig.dim = c(5L,
                                 16L), class = "data.frame")




attr(zz55ih, "label.table") <- label.table_attribute

usethis::use_data(zz55ih, internal = TRUE, overwrite = TRUE)

# it also needs to be a .dta to be read in the tests
readstata13::save.dta13(zz55ih, file = here::here("tests", "testdata", "zz55ih.dta"))
