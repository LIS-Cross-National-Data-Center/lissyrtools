# zz55ip.R

#' Mock dataset with person-level LIS format.
#'
#' Contains 5 rows of artificial data, with value labels and same attributes
#'   as a LIS file. The attributes are similar to those in the LIS it14ip sample
#'   file from the self-teaching datasets
#'   https://www.lisdatacenter.org/resources/self-teaching/

library(here)

# read the label.table attribute from another script
label.table_attribute <- source(here("data-raw", "label_table_zz55ip.R"))

zz55ip <- structure(list(hid = c(1L, 2L, 2L, 3L, 3L),
               pitotal = c(20000, 22000, 24000, 26000, 35000),
               emp = structure(c(1L, 1L, 1L, 1L, 2L),
                               .Label = c("[0]not employed", "[1]employed"), class = "factor"),
               did = c(999L, 999L, 999L, 999L, 999L),
               dname = c("zz55", "zz55", "zz55", "zz55", "zz55"),
               cname = c("ZZ", "ZZ", "ZZ", "ZZ", "ZZ"),
               iso2 = c("zz", "zz", "zz", "zz",  "zz"),
               iso3 = c("zzz", "zzz", "zzz", "zzz", "zzz"),
               year = c(5555L, 5555L, 5555L, 5555L, 5555L),
               wave = structure(c(10L, 10L, 10L, 10L, 10L),
                                .Label = c("[0]Historical Wave", "[1]Wave I",
                                           "[2]Wave II", "[3]Wave III", "[4]Wave IV", "[5]Wave V", "[6]Wave VI",
                                           "[7]Wave VII", "[8]Wave VIII", "[9]Wave IX", "[10]Wave X",
                                           "[11]Wave XI", "[12]Wave XII", "[13]Wave XIII"), class = "factor"),
               grossnet = structure(c(4L, 4L, 4L, 4L, 4L),
                                    .Label = c("[100]gross, taxes and contributions fully captured",
                                               "[110]gross, taxes and contributions collected",
                                               "[120]gross, taxes and contributions imputed",
                                               "[200]net, taxes and contributions not captured",
                                               "[300]mixed, taxes and contributions insufficiently captured",
                                               "[310]mixed, total income account for full taxes and contributions, subcomponents do not",
                                               "[320]mixed, total income does not account for full taxes and contributions"),
                                    class = "factor"),
               age = c(20L, 30L, 40L, 50L, 60L),
               sex = structure(c(1L, 2L, 1L, 2L, 2L),
                       .Label = c("[1]male", "[2]female"), class = "factor"),
               relation = structure(c(1L, 1L, 2L, 1L, 3L),
                                    .Label = c("[1000]head",
                                               "[2000]spouse/partner", "[2100]spouse", "[2200]cohabiting partner",
                                               "[3000]child", "[3100]own child (incl adopted)", "[3200]step-child",
                                               "[3300]foster child", "[4000]other", "[4100]other relative",
                                               "[4110]spouse/partner of child", "[4120]grandchild or greatgrandchild (incl in-laws)",
                                               "[4130]parent/grandparent/ascendant (incl in-laws)", "[4131]parent/grandparent/ascendant",
                                               "[4132]parent/grandparent/ascendant-in-law", "[4140]siblings (incl in-laws)",
                                               "[4150]aunt/uncle", "[4160]nephew/niece", "[4170]cousin",
                                               "[4200]other non-relative", "[4210]housemate/roommate", "[4220]domestic employee and his/her family",
                                               "[4230]guest/visitor/boarder/lodger"), class = "factor"),
               pwgt = c(0.22, 0.25, 0.26, 0.28, 0.37),
               ppopwgt = c(1400, 1450, 700, 800, 1300),
               pwgta = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_,
                         NA_integer_),
               pid = c(1, 1, 2, 1, 2)),
          row.names = c(NA, -5L),
          datalabel = "zz55: version created on 31 Jan 2021",
          time.stamp = "31 Jan 2021 00:00",
          formats = c("%8.0g", "%16.0f", "%15.0g", "%8.0g", "%9s",
                      "%9s", "%9s", "%9s", "%8.0g", "%18.0g", "%87.0g",
                      "%23.0g", "%8.0g", "%9.0g", "%50.0g", "%10.0g",
                      "%10.0g", "%8.0g", "%9.0g"),
          types = c(65529L, 65526L, 65529L,
                    65529L, 4L, 5L, 2L, 3L, 65529L, 65529L, 65529L, 65530L, 65529L,
                    65530L, 65529L, 65526L, 65526L, 65530L, 65527L),
          val.labels = c("",
                         "", emp = "emp", "", "", "", "", "", "", wave = "wave", grossnet = "grossnet",
                         "", sex = "sex", relation = "relation", "",
                         "", "", ""),
          var.labels = c("household identifier", "total individual income, person",
                         "employed (dummy)", "unique country/year number", "country/year identifier",
                         "country name", "2-letter country abbreviation", "3-letter country abbreviation",
                         "reference year", "data wave", "gross/net income information",
                         "individual income imputation (dummy)", "age in years", "gender",
                         "relationship to household head", "normalised person weight",
                         "person weight", "additional person weight", "person identifier"),
          version = 555L,
          byteorder = "LSF",
          orig.dim = c(5L, 19L),
          class = "data.frame")


attr(zz55ip, "label.table") <- label.table_attribute



# usethis::use_data_raw("zz55ip")
usethis::use_data(zz55ip, internal = TRUE, overwrite = TRUE)


# it also needs to be a .dta to be read in the tests
readstata13::save.dta13(zz55ip, file = here::here("tests", "testdata", "zz55ip.dta"))


