# household_&_person_variables.R

    # revised on 2nd October 2024 due to template revision - Gonçalo Marques 
    

        # what about the replicate weights in LWS "hrwgt..." ? 

# ---- LIS Household file vars----

lis_household_variables <- c("hid", "dhi", "hitotal", "hifactor", "hitransfer", "hilabour",
                             "hi11", "hi12", "hi121", "hi13", "hi14", "hicapital", "hi21",
                             "hi22", "hipension", "hi31", "hi311", "hi312", "hi32", "hi33",
                             "hi331", "hi332", "hpublic", "hpub_i", "hpub_u", "hpub_a", "hipubsoc",
                             "hi41", "hi411", "hi412", "hi42", "hi421", "hi422", "hi43", "hi44",
                             "hi45", "hi46", "hi47", "hi471", "hiprivate", "hi51", "hi511",
                             "hi52", "hi521", "hi522", "hi53", "hi531", "hi532", "dhci", "hvalgs",
                             "hxitsc", "hxitax", "hxscont", "hxotax", "hxptax", "hxvcont",
                             "hxiht", "hxalim", "hxremit", "hxmort", "hxintm", "hxloan", "hxintl",
                             "hhouscost", "hrenti", "hcexp", "hc1", "hc2", "hc3", "hc4", "hc41",
                             "hc5", "hc6", "hc7", "hc8", "hc9", "hc10", "hc11", "hc12", "helabour",
                             "hecgain", "heinherit", "heoth", "nearn", "region_c", "locsz_c",
                             "area_c", "rural", "own", "dweltyp", "farming", "did", "dname",
                             "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet",
                             "nhhmem", "nhhmem65", "nhhmem17", "nhhmem13", "hhtype", "hpartner",
                             "hwgt", "hpopwgt", "hwgta" , "nrooms", "typehh", "nhhmem6")


# ---- LIS Person file vars----

lis_person_variables <- c("hid", "pid", "pitotal", "pilabour", "pi11", "pi12", "pi13",
                          "pipension", "pi31", "pi311", "pi312", "pi32", "pi332", "pi33",
                          "pi331", "ppub_i", "pi411", "pi42", "pi421", "pi422", "pi43",
                          "pi44", "pi511", "pelabour", "pxitax", "pxscont", "pxitsc", "pxvcont",
                          "lfs", "emp", "emp_ilo", "informal", "parleave", "secjob", "weeks",
                          "hours1", "hourstot", "ptime1", "status1", "temp1", "public1",
                          "ind1_c", "indd1", "indc1", "indb1", "inda1", "occ1_c", "occb1",
                          "occa1", "fyft", "wexptl", "enroll", "educ_c",
                          "educlev", "educ", "illiterate", "edyrs", "edmom_c", "eddad_c",
                          "ctrybrth", "citizen", "yrsresid", "ethnic_c", "migrat_c", "immigr_c",
                          "immigr", "disabled", "health_c", "did", "dname", "cname", "iso2",
                          "iso3", "year", "wave", "currency", "grossnet", "age",
                          "sex", "marital", "relation", "partner", "parents", "nchildren",
                          "ageyoch", "pwgt", "ppopwgt", "pwgta", "depchild", "oneparent", "momnum", 
                          "dadnum", "partnum", "wage1", "hwage1", "occc1", "weeksft")

# ---- LIS all vars----

lis_variables <- union(lis_household_variables, lis_person_variables)  

# ---- LIS common vars----

lis_both_hp_variables <- intersect(lis_household_variables, lis_person_variables)  


# ---- LIS Technical vars----

lis_technical_variables <- c("did", "dname", "cname", "iso2", "iso3", "year",
                             "wave", "currency", "grossnet") # dataset invariant 

# ---- LIS Weights vars----

lis_weight_variables <- lis_variables[stringr::str_sub(lis_variables,-3,-1) == "wgt" | stringr::str_sub(lis_variables,-4,-1) == "wgta"]

# ---- LIS ID's vars----

lis_id_variables <- c("hid", "pid")
 
# --- LIS Categorical vars ----

lis_categorical_variables <- c("region_c", "rural","locsz_c", "area_c", "own", "dweltyp", "partner", "hpartner", 
                               "hhtype", "typehh", "nhhmem", "nhhmem65", "nhhmem17", "nhhmem13", "nhhmem6", 
                               "nearn", "relation", "parents", "nchildren", "oneparent", "depchild", 
                               "sex", "marital", "immigr", "citizen", "ctrybrth", "disabled", "health_c", 
                               "educ", "educlev", "educ_c", "enroll", "illiterate", "emp", "emp_ilo", "lfs", 
                               "parleave", "fyft", "secjob", "status1", "inda1", "indb1", "indd1", "ind1_c", 
                               "public1", "occa1", "occc1", "occ1_c", "ptime1","farming", "temp1","indc1", "occb1", "edmom_c" , 
                               "eddad_c" , "ethnic_c","migrat_c", "immigr_c")

# --- LIS Continuous vars ----

lis_continuous_variables <- c("dhi", "hitotal", "hifactor", "hitransfer", "hilabour", "hi11", "hi12", "hi121", "hi13", 
                               "hi14", "hicapital", "hi21", "hi22", "hipension", "hi31", "hi311", "hi312", "hi32", 
                               "hi33", "hi331", "hi332", "hpublic", "hpub_i", "hpub_u", "hpub_a", "hipubsoc", "hi41", 
                               "hi411", "hi412", "hi42", "hi421", "hi422", "hi43", "hi44", "hi45", "hi46", 
                               "hi47", "hi471", "hiprivate", "hi51", "hi511", "hi52", "hi521", "hi522", "hi53", 
                               "hi531", "hi532", "dhci", "hvalgs", "hxitsc", "hxitax", "hxscont", "hxotax", "hxptax", 
                               "hxvcont", "hxiht", "hxalim", "hxremit", "hxmort", "hxintm", "hxloan", "hxintl", "hhouscost", 
                               "hrenti", "hcexp", "hc1", "hc2", "hc3", "hc4", "hc41", "hc5", "hc6", 
                               "hc7", "hc8", "hc9", "hc10", "hc11", "hc12", "helabour", "hecgain", "heinherit", 
                               "heoth", "pitotal", "pilabour", "pi11", "pi12", 
                               "pi13", "pipension", "pi31", "pi311", "pi312", "pi32", "pi332", "pi33", "pi331", 
                               "ppub_i", "pi411", "pi42", "pi421", "pi422", "pi43", "pi44", "pi511", "pelabour", 
                               "pxitax", "pxscont", "pxitsc", "pxvcont", "informal", "weeks", "hours1", 
                               "hourstot", "wexptl", "yrsresid", "age", "ageyoch", "momnum", "dadnum", "partnum", "wage1", "hwage1", 
                               "weeksft", "nrooms" , "edyrs")


# --- LIS Country Specific vars ----

lis_country_specific_variables <- lis_variables[stringr::str_sub(lis_variables,-2,-1) == "_c"]


# --- LIS Income vars ----

lis_income_variables <- c("dhi", "hitotal", "hifactor", "hitransfer", "hilabour", "hi11", "hi12", "hi121", "hi13", 
                          "hi14", "hicapital", "hi21", "hi22", "hipension", "hi31", "hi311", "hi312", "hi32", 
                          "hi33", "hi331", "hi332", "hpublic", "hpub_i", "hpub_u", "hpub_a", "hipubsoc", "hi41", 
                          "hi411", "hi412", "hi42", "hi421", "hi422", "hi43", "hi44", "hi45", "hi46", 
                          "hi47", "hi471", "hiprivate", "hi51", "hi511", "hi52", "hi521", "hi522", "hi53", 
                          "hi531", "hi532", "dhci", "hvalgs", "hxitsc", "hxitax", "hxscont", "hxotax", "hxptax", 
                          "hxvcont", "hxiht", "hxalim", "hxremit", "hxmort", "hxintm", "hxloan", "hxintl", "hhouscost", 
                          "hrenti", "hcexp", "hc1", "hc2", "hc3", "hc4", "hc41", "hc5", "hc6", 
                          "hc7", "hc8", "hc9", "hc10", "hc11", "hc12", "helabour", "hecgain", "heinherit", 
                          "heoth", "pitotal", "pilabour", "pi11", "pi12", 
                          "pi13", "pipension", "pi31", "pi311", "pi312", "pi32", "pi332", "pi33", "pi331", 
                          "ppub_i", "pi411", "pi42", "pi421", "pi422", "pi43", "pi44", "pi511", "pelabour", 
                          "pxitax", "pxscont", "pxitsc", "pxvcont")


# ---- LWS Household file vars----

lws_household_variables <- c("hid", "inum", "cir", "cia", "cig", "chc", "cha", "cnc", "cna",
                             "ppr", "ppy", "pir", "pia1", "pia2", "pia3", "pia4", "piy1",
                             "piy2", "piy3", "piy4", "pit1", "pit2", "pit3", "pit4", "piw1",
                             "piw2", "piw3", "piw4", "pim1", "pim2", "pim3", "pim4", "bopc",
                             "bonc_c", "boue", "boea", "bolc", "bocc1_c", "bocc2_c", "bocs_c",
                             "bocd1_c", "bocd2_c", "boef_c", "boee1_c", "boee2_c", "bus1_c",
                             "bus2_c", "bus3_c", "dnw", "anw", "inw", "tnw", "hanrp", "hanro",
                             "hannb", "hanncv", "hanncd", "hafc", "hafib", "hafis", "hafii",
                             "hafo", "han", "hanr", "hann", "hannc", "hanno", "haf", "hafi",
                             "has", "hasi", "hasil", "hasip", "haso", "hasodb", "hasodc",
                             "hass", "hassdb", "hassdc", "ha", "hlrp", "hlro", "hlni", "hlncv",
                             "hlncd", "hlne", "hlno", "hlr", "hln", "hlnc", "hl", "hlsrp",
                             "hlsro", "hlsnn", "hlsni", "hlsn", "hlsr", "hlsng", "dhi", "hitotal",
                             "hifactor", "hitransfer", "hilabour", "hi11", "hi12", "hi121",
                             "hi13", "hi14", "hicapital", "hi21", "hi22", "hipension", "hi31",
                             "hi311", "hi312", "hi32", "hi33", "hi331", "hi332", "hpublic",
                             "hpub_i", "hpub_u", "hpub_a", "hipubsoc", "hi41", "hi411", "hi412",
                             "hi42", "hi421", "hi422", "hi43", "hi44", "hi45", "hi46", "hi47",
                             "hi471", "hiprivate", "hi51", "hi511", "hi52", "hi521", "hi522",
                             "hi53", "hi531", "hi532", "dhci", "hvalgs", "hxitsc", "hxitax",
                             "hxscont", "hxotax", "hxptax", "hxvcont", "hxiht", "hxalim",
                             "hxremit", "hxmort", "hxintm", "hxloan", "hxintl", "hhouscost",
                             "hrenti", "hcexp", "hc1", "hc2", "hc3", "hc4", "hc41", "hc5",
                             "hc6", "hc7", "hc8", "hc9", "hc10", "hc11", "hc12", "helabour",
                             "hecgain", "heinherit", "heoth", "nearn", "region_c", "locsz_c",
                             "area_c", "rural", "own", "dweltyp", "farming", "did", "dname",
                             "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet",
                             "nhhmem", "nhhmem65", "nhhmem17", "nhhmem13", "hhtype", "hpartner",
                             "hwgt", "hpopwgt", "hwgta", "nrooms", "typehh", 
                             "nhhmem6", "hafct", "hafcs", "hafiss", "hafiso", "hafom")

# ---- LWS Person file vars----

lws_person_variables <- c("hid", "inum", "pid", "basb", "basp1", "basp2", "basp3", "basp4",
                          "basp5", "basp6", "basp7", "basp8", "basp9", "bafr1_c", "bafr2_c",
                          "bafr3_c", "bafp1_c", "bafp2_c", "bafl1_c", "bafl2_c", "bafl3_c",
                          "bafl4_c", "bafi1_c", "bafi2_c", "bafi3_c", "bafi4_c", "bafi5_c",
                          "ssyc", "pasi", "pasodc", "paso", "pass", "pas", "pasil", "pasip",
                          "pasodb", "passdb", "passdc", "pitotal", "pilabour", "pi11",
                          "pi12", "pi13", "pipension", "pi31", "pi311", "pi312", "pi32",
                          "pi33", "pi331", "pi332", "ppub_i", "pi411", "pi42", "pi421",
                          "pi422", "pi43", "pi44", "pi511", "pelabour", "pxitax", "pxscont",
                          "pxitsc", "pxvcont", "lfs", "emp", "emp_ilo", "informal", "parleave",
                          "secjob", "weeks", "hours1", "hourstot", "ptime1", "status1",
                          "temp1", "public1", "ind1_c", "indd1", "indc1", "indb1", "inda1",
                          "occ1_c", "occb1", "occa1", "fyft", "wexptl",
                          "enroll", "educ_c", "educlev", "educ", "illiterate", "edyrs",
                          "edmom_c", "eddad_c", "ctrybrth", "citizen", "yrsresid", "ethnic_c",
                          "migrat_c", "immigr_c", "immigr", "disabled", "health_c", "did",
                          "dname", "cname", "iso2", "iso3", "year", "wave", "currency",
                          "grossnet", "age", "sex", "marital", "relation", "partner",
                          "parents", "nchildren", "ageyoch", "pwgt", "ppopwgt", "pwgta", "depchild", 
                          "oneparent", "momnum", "dadnum", "partnum", "wage1", "hwage1", "occc1", "weeksft") 


# ---- LWS all vars----

lws_variables <- union(lws_household_variables, lws_person_variables)  

# ---- LWS common vars----

lws_both_hp_variables <- intersect(lws_household_variables, lws_person_variables)  



# ---- LWS ID's vars----

lws_id_variables <- c("hid", "pid", "inum")



# --- LWS wealth categorical vars ----

lws_wealth_categorical_variables <- c("cna", "cnc", "cir", "cia", "cig", "chc", "cha", "ppy", "pir", "pia1", "pia2", 
  "pia3", "pia4", "piy1", "piy2", "piy3", "piy4", "pit1", "pit2", "pit3", "pit4", 
  "piw1", "piw2", "piw3", "piw4", "pim1", "pim2", "pim3", "pim4", "ssyc", "bus1_c", 
  "bus2_c", "bus3_c", "basb", "basp1", "basp2", "basp3", "basp4", "basp5", "basp6", 
  "basp7", "basp8", "basp9", "bafr1_c", "bafr2_c", "bafr3_c", "bafp1_c", "bafp2_c", 
  "bafl1_c", "bafl2_c", "bafl3_c", "bafl4_c", "bafi1_c", "bafi2_c", "bafi3_c", "bafi4_c", 
  "bafi5_c", "bopc", "bonc_c", "boue", "boea", "bolc", "bocc1_c", "bocc2_c", "bocs_c", 
  "bocd1_c", "bocd2_c", "boef_c", "boee1_c", "boee2_c")

# --- LWS wealth continuous vars ----

lws_wealth_continuous_variables <- c(
  "ppr", "dnw", "anw", "inw", "tnw", "hanrp", "hanro", "hanr", "hannb", "hanncv", "hanncd", 
  "hafc", "hafib", "hafiss", "hafiso", "hafii", "hafom", "han", "hann", "hannc", "hanno", "haf", 
  "hafct", "hafcs", "hafi", "hafis", "hafo", "has", "hasi", "hasil", "hasip", "haso", "hasodb", 
  "hasodc", "hass", "hassdb", "hassdc", "ha", "hlrp", "hlro", "hlncv", "hlncd", "hlne", "hlno", 
  "hlr", "hln", "hlni", "hlnc", "hl", "hlsr", "hlsni", "hlsn", "hlsrp", "hlsro", "hlsng", "hlsnn", 
  "pasil", "pasip", "pasodc", "pass", "pas", "pasi", "paso", "pasodb", "passdb", "passdc"
)

# --- LWS wealth country-specific vars ----  

lws_wealth_country_specific_variables <- lws_wealth_categorical_variables[stringr::str_sub(lws_wealth_categorical_variables,-2,-1) == "_c"]

# ---- ERFLIS Household file vars----

erflis_household_variables <- c("hid", "dhi", "hitotal", "hifactor", "hitransfer", "hilabour",
                               "hi11", "hi12", "hi121", "hi13", "hi14", "hicapital", "hi21",
                               "hi22", "hipension", "hi31", "hi311", "hi312", "hi32", "hi33",
                               "hi331", "hi332", "hpublic", "hpub_i", "hpub_u", "hpub_a", "hipubsoc",
                               "hi41", "hi411", "hi412", "hi42", "hi421", "hi422", "hi43", "hi44",
                               "hi45", "hi46", "hi47", "hi471", "hiprivate", "hi51", "hi511",
                               "hi52", "hi521", "hi522", "hi53", "hi531", "hi532", "dhci", "hvalgs",
                               "hxitsc", "hxitax", "hxscont", "hxotax", "hxptax", "hxvcont",
                               "hxiht", "hxalim", "hxremit", "hxmort", "hxintm", "hxloan", "hxintl",
                               "hhouscost", "hrenti", "hcexp", "hc1", "hc2", "hc3", "hc4", "hc41",
                               "hc5", "hc6", "hc7", "hc8", "hc9", "hc10", "hc11", "hc12", "helabour",
                               "hecgain", "heinherit", "heoth", "nearn", "region_c", "locsz_c",
                               "area_c", "rural", "own", "dweltyp", "farming", "did", "dname",
                               "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet",
                               "nhhmem", "nhhmem65", "nhhmem17", "nhhmem13", "hhtype", "hpartner",
                               "hwgt", "hpopwgt", "hwgta", "elect", "slight",
                               "scook", "wat", "tfacil", "sfacil", "hfacil", "toif", "toif_sh",
                               "grbg_dsp", "car", "car_n", "truck", "truck_n", "mbcycle", "mbcycle_n",
                               "telv", "telv_n", "radio", "radio_n", "satd_rec", "satd_rec_n",
                               "player", "player_n", "camera", "camera_n", "game", "game_n",
                               "telph", "telph_n", "fax", "fax_n", "computer", "computer_n",
                               "internet", "internet_n", "refrg", "refrg_n", "cooker", "cooker_n",
                               "microwave", "microwave_n", "skitapp", "skitapp_n", "wash", "wash_n",
                               "dryer", "dryer_n", "dshwsh", "dshwsh_n", "cond", "cond_n", "fan",
                               "fan_n", "heater", "heater_n", "waheat", "waheat_n", "sewing",
                               "sewing_n", "vacuum", "vacuum_n", "iron", "iron_n", "odur", "odur_n",
                               "livestok", "livestok_n", "rice", "grain", "bread", "past", "cerpre",
                               "cerown", "smeat", "cmeat", "omeat", "chmeat", "lmeat", "meatown",
                               "fish", "seaf", "fshseaown", "milk", "yogh", "ches", "egg", "dairyown",
                               "ooil", "oil", "but", "fat", "oilfatown", "fruitexp", "fruitown",
                               "vegexp", "vegown", "sugarexp", "sugarown", "spicexp", "spicown",
                               "bevexp", "bevown", "othfexp", "othfown", "totgikd", "alc", "tob",
                               "cloth", "mclo", "char", "footw", "schclth", "repdw", "wdwe",
                               "edwe", "hsfur", "furrep", "hstex", "mhapl", "shapl", "rappl",
                               "hust", "mhope", "shope", "hsgs", "pmedp", "taeq", "medserv",
                               "medabr", "trveh", "oteq", "trserv", "schserv", "pttserv", "tteqp",
                               "culdur", "culeqp", "culser", "schcul", "preduc", "seduc", "teduc",
                               "oeduc", "educabr", "catser", "schrst", "accom", "pcexp", "spexp",
                               "fexp", "fees", "ptransf", "nrooms", "typehh", "nhhmem6")

# ---- ERFLIS Person file vars----

erflis_person_variables <- c("hid", "pid", "pitotal", "pilabour", "pi11", "pi12", "pi13",
                            "pipension", "pi31", "pi311", "pi312", "pi32", "pi33", "pi331",
                            "pi332", "ppub_i", "pi411", "pi42", "pi421", "pi422", "pi43",
                            "pi44", "pi511", "pelabour", "pxitax", "pxscont", "pxitsc", "pxvcont",
                            "lfs", "emp", "emp_ilo", "informal", "parleave", "secjob", "weeks",
                            "hours1", "hourstot", "ptime1", "status1", "temp1", "public1",
                            "ind1_c", "indd1", "indc1", "indb1", "inda1", "occ1_c", "occb1",
                            "occa1", "fyft", "wexptl", "enroll", "educ_c",
                            "educlev", "educ", "illiterate", "edyrs", "edmom_c", "eddad_c",
                            "ctrybrth", "citizen", "yrsresid", "ethnic_c", "migrat_c", "immigr_c",
                            "immigr", "disabled", "health_c", "did", "dname", "cname", "iso2",
                            "iso3", "year", "wave", "currency", "grossnet", "age",
                            "sex", "marital", "relation", "partner", "parents", "nchildren",
                            "ageyoch", "pwgt", "ppopwgt", "pwgta", "depchild", "oneparent", 
                            "momnum", "dadnum", "partnum", "wage1", "hwage1", "occc1", "weeksft")


# ---- Keys ----

key_vars_household_lis <- c("hid", "nhhmem", "hwgt", "hpopwgt", "hwgta","year","iso2", "dname", "currency")
key_vars_person_lis <- c("hid", "pid", "relation", "pwgt", "ppopwgt", "pwgta","year","iso2", "dname", "currency")

key_vars_household_lws <- c("hid", "inum", "nhhmem", "hwgt", "hpopwgt", "hwgta","year","iso2", "dname", "currency")
key_vars_person_lws <- c("hid", "inum", "pid", "relation", "pwgt", "ppopwgt", "pwgta","year","iso2", "dname", "currency")



# ---- Usethis ----


usethis::use_data(lis_household_variables,
                  lis_person_variables,
                  lis_variables,
                  lis_both_hp_variables,
                  lis_technical_variables,
                  lis_weight_variables,
                  lis_id_variables,
                  lis_categorical_variables,
                  lis_continuous_variables,
                  lis_country_specific_variables,
                  lis_income_variables,
                  lws_household_variables,
                  lws_person_variables,
                  lws_variables,
                  lws_both_hp_variables,
                  lws_id_variables, 
                  lws_wealth_categorical_variables,
                  lws_wealth_continuous_variables,
                  lws_wealth_country_specific_variables,
                  erflis_household_variables,
                  erflis_person_variables,
                  key_vars_household_lis,
                  key_vars_person_lis,
                  key_vars_household_lws,
                  key_vars_person_lws,
                  internal = TRUE, # to save on R/sysdata.rda
                  overwrite = TRUE)


