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
                               "eddad_c" , "informal",  "ethnic_c","migrat_c", "immigr_c")

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
                               "pxitax", "pxscont", "pxitsc", "pxvcont", "weeks", "hours1", 
                               "hourstot", "wexptl", "yrsresid", "age", "ageyoch", "wage1", "hwage1", 
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



# --- LCS Household file vars ----
lcs_household_variables <- c("hid", "did", "dname", "cname", "iso2", "iso3", "year", "wave", "hpopwgt", "hwgt", "hwgta", "currency", "grossnet", "region_c",
                             "rural", "locsz_c", "area_c", "own", "dweltyp", "hhtype", "hpartner", "nhhmem", "nhhmem65", "nhhmem17", "nhhmem13", "nearn", "hitotal", "dhi",
                             "hvalgs", "dhci", "hifactor", "hitransfer", "hpublic", "hpub_i", "hpub_u", "hpub_a", "hilabour", "hi11", "hi12", "hi121", "hi13", "hi14",
                             "hicapital", "hi21", "hi22", "hipension", "hi31", "hi311", "hi312", "hi32", "hi33", "hi331", "hi332", "hipubsoc", "hi41", "hi411",
                             "hi412", "hi42", "hi421", "hi422", "hi43", "hi44", "hi45", "hi46", "hi47", "hi471", "hiprivate", "hi51", "hi511", "hi52",
                             "hi521", "hi522", "hi53", "hi531", "hi532", "hxitsc", "hxitax", "hxscont", "hxvcont", "helabour", "hecgain", "heinherit", "heoth", "hrenti",
                             "nrooms", "typehh", "nhhmem6", "dint", "dwelsqm", "dwelyrbuilt_c", "dweltenu", "matroof_c", "matfloor_c", "matwalls_c", "toiletshrd", "dwelsec", "own_c", "dwelcval",
                             "dwelacqui_c", "dwelyrpurchd", "landlord_c", "rent_c", "furnished", "swater", "watdrink_c", "selectr", "cenergy", "henergy", "renergy", "grbgcoll", "grbgdisp_c", "sewage",
                             "toiletfac", "heating", "hotwater", "shotwater_c", "aircon", "wataccs_c", "healthaccs_c", "schoolaccs_c", "transaccs_c", "internet", "tvsub", "areaqual1_c", "areaqual2_c", "areaqual3_c",
                             "areaqual4_c", "dwelqual1_c", "dwelqual2_c", "dwelqual3_c", "dwelqual4_c", "dwelprob", "adq_hous_c", "adq_clth_c", "adq_food1_c", "adq_food2_c", "adq_food3_c", "adq_food4_c", "food_qnt1_c", "food_qnt2_c",
                             "food_qnt3_c", "food_qnt4_c", "adq1_c", "adq2_c", "adq3_c", "adq4_c", "fin_satf_c", "fin_chng_c", "fin_expc_c", "saves", "saving_c", "dissaving_c", "min_income", "unexp_exp",
                             "povrank_c", "shocks1_c", "shocks2_c", "shocks3_c", "shocks4_c", "hc_lcs", "hc_p", "hc_o", "hc_i", "hc_m", "hcfood", "hchous", "hchous_t", "hchous_n",
                             "hcnfnd", "hcfood_p", "hcfood_o", "hcfood_i", "hchous_p", "hchous_o", "hchous_i", "hcnfnd_p", "hcnfnd_o", "hcnfnd_i", "hc_dflow", "hc_lcs_ext", "hd05_p", "hd06_p",
                             "hd07_p", "hd08_p", "hd09_p", "hd13_p", "hxother", "hxdon", "hc_coicop_t", "hc_coicop_p", "hc_coicop_n", "hc01_t", "hc02_t", "hc03_t", "hc04_t", "hc05_t",
                             "hc06_t", "hc07_t", "hc08_t", "hc09_t", "hc10_t", "hc11_t", "hc12_t", "hc13_t", "hc99_t", "hcfood_t", "hcfood_n", "hcalcto_t", "hcalcto_p", "hcalcto_n",
                             "hccloth_t", "hccloth_p", "hccloth_n", "hcequip_t", "hcequip_p", "hcequip_n", "hctrans_t", "hctrans_p", "hctrans_n", "hcinco_t", "hcinco_p", "hcinco_n", "hcrsc_t", "hcrsc_p",
                             "hcrsc_n", "hcrest_t", "hcrest_p", "hcrest_n", "hcaccs_t", "hcaccs_p", "hcaccs_n", "hcins_t", "hcins_p", "hcins_n", "hcpers_t", "hcpers_p", "hcpers_n", "hcsoc_t",
                             "hcsoc_p", "hcsoc_n", "hcmisc_t", "hcmisc_p", "hcmisc_n", "hd05_t", "hd05_n", "hd06_t", "hd06_n", "hd07_t", "hd07_n", "hd08_t", "hd08_n", "hd09_t",
                             "hd09_n", "hd13_t", "hd13_n", "hc01_p", "hc01_n", "hc02_p", "hc02_n", "hc03_p", "hc03_n", "hc04_p", "hc04_n", "hc05_p", "hc05_n", "hc06_p",
                             "hc06_n", "hc07_p", "hc07_n", "hc08_p", "hc08_n", "hc09_p", "hc09_n", "hc10_p", "hc10_n", "hc11_p", "hc11_n", "hc12_p", "hc12_n", "hc13_p",
                             "hc13_n", "hc99_p", "hc99_n", "hd0613_t", "hd0613_p", "hd0613_n", "hc081_t", "hc081_p", "hc081_n", "hd1321_t", "hd1321_p", "hd1321_n", "hc_ie", "hc_is",
                             "hc_ip", "hc_ih", "hc_im", "hcfood_ie", "hcfood_is", "hcfood_ip", "hcfood_ih", "hchous_ie", "hchous_is", "hchous_ip", "hchous_ih", "hcnfnd_ie", "hcnfnd_is", "hcnfnd_ip",
                             "hcnfnd_ih", "hf05", "hf06", "hf07", "hf08", "hf09", "hxvcontp", "hxvconth", "hxvcontu", "hxvconto", "hxmdon", "hxmfee", "hxndon", "hxomihtr",
                             "hxomihtn", "hxonihtr", "hxonihtn", "hd0511_t", "hd0511_p", "hd0511_n", "hd0531_t", "hd0531_p", "hd0531_n", "hd0551_t", "hd0551_p", "hd0551_n", "hd0711_t", "hd0711_p",
                             "hd0711_n", "hd0712_t", "hd0712_p", "hd0712_n", "hd0713_t", "hd0713_p", "hd0713_n", "hd0714_t", "hd0714_p", "hd0714_n", "hd0719_t", "hd0719_p", "hd0719_n", "hd0811_t",
                             "hd0811_p", "hd0811_n", "hd0812_t", "hd0812_p", "hd0812_n", "hd0813_t", "hd0813_p", "hd0813_n", "hd0814_t", "hd0814_p", "hd0814_n", "hd0819_t", "hd0819_p", "hd0819_n",
                             "hd0911_t", "hd0911_p", "hd0911_n", "hd0912_t", "hd0912_p", "hd0912_n", "hd09321_t", "hd09321_p", "hd09321_n", "hd0951_t", "hd0951_p", "hd0951_n", "hc011_t", "hc011_p",
                             "hc011_n", "hc012_t", "hc012_p", "hc012_n", "hc013_t", "hc013_p", "hc013_n", "hc021_t", "hc021_p", "hc021_n", "hc022_t", "hc022_p", "hc022_n", "hc023_t",
                             "hc023_p", "hc023_n", "hc024_t", "hc024_p", "hc024_n", "hc031_t", "hc031_p", "hc031_n", "hc032_t", "hc032_p", "hc032_n", "hc041_t", "hc041_p", "hc041_n",
                             "hc042_t", "hc042_p", "hc042_n", "hc043_t", "hc043_p", "hc043_n", "hc044_t", "hc044_p", "hc044_n", "hc045_t", "hc045_p", "hc045_n", "hc051_t", "hc051_p",
                             "hc051_n", "hc052_t", "hc052_p", "hc052_n", "hc053_t", "hc053_p", "hc053_n", "hc054_t", "hc054_p", "hc054_n", "hc055_t", "hc055_p", "hc055_n", "hc056_t",
                             "hc056_p", "hc056_n", "hc061_t", "hc061_p", "hc061_n", "hc062_t", "hc062_p", "hc062_n", "hc063_t", "hc063_p", "hc063_n", "hc064_t", "hc064_p", "hc064_n",
                             "hc071_t", "hc071_p", "hc071_n", "hc072_t", "hc072_p", "hc072_n", "hc073_t", "hc073_p", "hc073_n", "hc074_t", "hc074_p", "hc074_n", "hc082_t", "hc082_p",
                             "hc082_n", "hc083_t", "hc083_p", "hc083_n", "hc091_t", "hc091_p", "hc091_n", "hc092_t", "hc092_p", "hc092_n", "hc093_t", "hc093_p", "hc093_n", "hc094_t",
                             "hc094_p", "hc094_n", "hc095_t", "hc095_p", "hc095_n", "hc096_t", "hc096_p", "hc096_n")

# --- LCS Person file vars ----
lcs_person_variables <- c("hid", "pid", "did", "dname", "cname", "iso2", "iso3", "year", "wave", "ppopwgt", "pwgt", "pwgta", "currency", "grossnet", "relation", "partner", "parents", "nchildren",
                          "ageyoch", "age", "sex", "marital", "immigr", "citizen", "ctrybrth", "yrsresid", "ethnic_c", "migrat_c", "immigr_c", "disabled", "health_c", "educ", "educlev", "educ_c", "enroll", "edyrs",
                          "illiterate", "edmom_c", "eddad_c", "emp", "emp_ilo", "lfs", "informal", "parleave", "fyft", "hourstot", "weeks", "secjob", "wexptl", "status1", "inda1", "indb1", "indc1", "indd1",
                          "ind1_c", "public1", "occa1", "occb1", "occ1_c", "temp1", "ptime1", "hours1", "ppub_i", "pitotal", "pilabour", "pi11", "pi12", "pi13", "pipension", "pi31", "pi311", "pi312",
                          "pi32", "pi33", "pi331", "pi332", "pi411", "pi42", "pi421", "pi422", "pi43", "pi44", "pi511", "pxitsc", "pxitax", "pxscont", "pxvcont", "pelabour", "oneparent", "depchild",
                          "momnum", "partnum", "dadnum", "occc1", "wage1", "hwage1", "weeksft", "dint", "health1_c", "health2_c", "health3_c", "pxvcontp", "pxvconth", "pxvcontu", "pxvconto")

# --- LCS all vars ----
lcs_variables <- union(lcs_household_variables, lcs_person_variables) 

# --- LCS common vars ----
lcs_both_hp_variables <- intersect(lcs_household_variables, lcs_person_variables)  

# --- LCS Technical vars ----
lcs_technical_variables <- c("did", "dname", "cname", "iso2", "iso3", "year",
                             "wave", "currency", "grossnet")
# --- LCS Weights vars ----
lcs_weight_variables <- lcs_variables[stringr::str_sub(lcs_variables,-3,-1) == "wgt" | stringr::str_sub(lcs_variables,-4,-1) == "wgta"]

# --- LCS ID's vars ----
lcs_id_variables <- c("hid", "pid")

# --- LCS Categorical vars ----
lcs_categorical_variables <- c("region_c", "rural", "locsz_c", "area_c", "own", "dweltyp", "hhtype", "hpartner", "nhhmem", "nhhmem65", "nhhmem17", "nhhmem13", "nearn", "typehh",
                               "nhhmem6", "dwelyrbuilt_c", "matroof_c", "matfloor_c", "matwalls_c", "toiletshrd", "dwelsec", "own_c", "dwelacqui_c", "dwelyrpurchd", "landlord_c", "rent_c", "furnished", "swater",
                               "watdrink_c", "selectr", "cenergy", "henergy", "renergy", "grbgcoll", "grbgdisp_c", "sewage", "toiletfac", "heating", "hotwater", "shotwater_c", "aircon", "wataccs_c",
                               "healthaccs_c", "schoolaccs_c", "transaccs_c", "internet", "tvsub", "areaqual1_c", "areaqual2_c", "areaqual3_c", "areaqual4_c", "dwelqual1_c", "dwelqual2_c", "dwelqual3_c", "dwelqual4_c", "dwelprob",
                               "adq_hous_c", "adq_clth_c", "adq_food1_c", "adq_food2_c", "adq_food3_c", "adq_food4_c", "food_qnt1_c", "food_qnt2_c", "food_qnt3_c", "food_qnt4_c", "adq1_c", "adq2_c", "adq3_c", "adq4_c",
                               "fin_satf_c", "fin_chng_c", "fin_expc_c", "saves", "saving_c", "dissaving_c", "unexp_exp", "povrank_c", "shocks1_c", "shocks2_c", "shocks3_c", "shocks4_c", "relation", "partner",
                               "parents", "nchildren", "sex", "marital", "immigr", "citizen", "ctrybrth", "ethnic_c", "migrat_c", "immigr_c", "disabled", "health_c", "educ", "educlev",
                               "educ_c", "enroll", "illiterate", "edmom_c", "eddad_c", "emp", "emp_ilo", "lfs", "informal", "parleave", "fyft", "secjob", "status1", "inda1",
                               "indb1", "indc1", "indd1", "ind1_c", "public1", "occa1", "occb1", "occ1_c", "temp1", "ptime1", "oneparent", "depchild", "occc1", "health1_c",
                               "health2_c", "health3_c")

# --- LCS Continuous vars ----
lcs_continuous_variables <- c("hitotal", "dhi", "hvalgs", "dhci", "hifactor", "hitransfer", "hpublic", "hpub_i", "hpub_u", "hpub_a", "hilabour", "hi11", "hi12", "hi121", "hi13", "hi14", "hicapital",
                              "hi21", "hi22", "hipension", "hi31", "hi311", "hi312", "hi32", "hi33", "hi331", "hi332", "hipubsoc", "hi41", "hi411", "hi412", "hi42", "hi421", "hi422",
                              "hi43", "hi44", "hi45", "hi46", "hi47", "hi471", "hiprivate", "hi51", "hi511", "hi52", "hi521", "hi522", "hi53", "hi531", "hi532", "hxitsc", "hxitax",
                              "hxscont", "hxvcont", "helabour", "hecgain", "heinherit", "heoth", "hrenti", "nrooms", "dwelsqm", "dweltenu", "dwelcval", "min_income", "hc_lcs", "hc_p", "hc_o", "hc_i", "hc_m",
                              "hcfood", "hchous", "hchous_t", "hchous_n", "hcnfnd", "hcfood_p", "hcfood_o", "hcfood_i", "hchous_p", "hchous_o", "hchous_i", "hcnfnd_p", "hcnfnd_o", "hcnfnd_i", "hc_dflow", "hc_lcs_ext", "hd05_p",
                              "hd06_p", "hd07_p", "hd08_p", "hd09_p", "hd13_p", "hxother", "hc_coicop_p", "hc_coicop_n", "hc01_t", "hc02_t", "hc03_t", "hc04_t", "hc05_t", "hc06_t", "hc07_t", "hc08_t", "hc09_t",
                              "hc10_t", "hc11_t", "hc12_t", "hc13_t", "hc99_t", "hcfood_t", "hcfood_n", "hcalcto_t", "hcalcto_p", "hcalcto_n", "hccloth_t", "hccloth_p", "hccloth_n", "hcequip_t", "hcequip_p", "hcequip_n", "hctrans_t",
                              "hctrans_p", "hctrans_n", "hcinco_t", "hcinco_p", "hcinco_n", "hcrsc_t", "hcrsc_p", "hcrsc_n", "hcrest_t", "hcrest_p", "hcrest_n", "hcaccs_t", "hcaccs_p", "hcaccs_n", "hcins_t", "hcins_p", "hcins_n",
                              "hcpers_t", "hcpers_p", "hcpers_n", "hcsoc_t", "hcsoc_p", "hcsoc_n", "hcmisc_t", "hcmisc_p", "hcmisc_n", "hd05_t", "hd05_n", "hd06_t", "hd06_n", "hd07_t", "hd07_n", "hd08_t", "hd08_n",
                              "hd09_t", "hd09_n", "hd13_t", "hd13_n", "hc01_p", "hc01_n", "hc02_p", "hc02_n", "hc03_p", "hc03_n", "hc04_p", "hc04_n", "hc05_p", "hc05_n", "hc06_p", "hc06_n", "hc07_p",
                              "hc07_n", "hc08_p", "hc08_n", "hc09_p", "hc09_n", "hc10_p", "hc10_n", "hc11_p", "hc11_n", "hc12_p", "hc12_n", "hc13_p", "hc13_n", "hc99_p", "hc99_n", "hd0613_t", "hd0613_p",
                              "hd0613_n", "hc081_t", "hc081_p", "hc081_n", "hd1321_t", "hd1321_p", "hd1321_n", "hc_ie", "hc_is", "hc_ip", "hc_ih", "hc_im", "hcfood_ie", "hcfood_is", "hcfood_ip", "hcfood_ih", "hchous_ie",
                              "hchous_is", "hchous_ip", "hchous_ih", "hcnfnd_ip", "hcnfnd_ih", "hf05", "hf06", "hf07", "hf08", "hf09", "hxvcontp", "hxvconth", "hxvcontu", "hxvconto", "hxmdon", "hxmfee", "hxndon",
                              "hxomihtr", "hxomihtn", "hxonihtr", "hxonihtn", "hd0511_t", "hd0511_p", "hd0511_n", "hd0531_t", "hd0531_p", "hd0531_n", "hd0551_t", "hd0551_p", "hd0551_n", "hd0711_t", "hd0711_p", "hd0711_n", "hd0712_t",
                              "hd0712_p", "hd0712_n", "hd0713_t", "hd0713_p", "hd0713_n", "hd0714_t", "hd0714_p", "hd0714_n", "hd0719_t", "hd0719_p", "hd0719_n", "hd0811_t", "hd0811_p", "hd0811_n", "hd0812_t", "hd0812_p", "hd0812_n",
                              "hd0813_t", "hd0813_p", "hd0813_n", "hd0814_t", "hd0814_p", "hd0814_n", "hd0819_t", "hd0819_p", "hd0819_n", "hd0911_t", "hd0911_p", "hd0911_n", "hd0912_t", "hd0912_p", "hd0912_n", "hd09321_t", "hd09321_p",
                              "hd09321_n", "hd0951_t", "hd0951_p", "hd0951_n", "hc011_t", "hc011_p", "hc011_n", "hc012_t", "hc012_p", "hc012_n", "hc013_t", "hc013_p", "hc013_n", "hc021_t", "hc022_t", "hc022_p", "hc022_n",
                              "hc023_t", "hc023_p", "hc023_n", "hc024_t", "hc024_p", "hc024_n", "hc031_t", "hc031_p", "hc031_n", "hc032_t", "hc032_p", "hc032_n", "hc041_t", "hc041_p", "hc041_n", "hc042_t", "hc042_p",
                              "hc042_n", "hc043_t", "hc043_p", "hc043_n", "hc044_t", "hc044_p", "hc044_n", "hc045_t", "hc045_p", "hc045_n", "hc051_t", "hc051_p", "hc051_n", "hc052_t", "hc052_p", "hc052_n", "hc053_t",
                              "hc053_p", "hc053_n", "hc054_t", "hc054_p", "hc054_n", "hc055_t", "hc055_p", "hc055_n", "hc056_t", "hc056_p", "hc056_n", "hc061_t", "hc061_p", "hc061_n", "hc062_t", "hc062_p", "hc062_n",
                              "hc063_t", "hc063_p", "hc063_n", "hc064_t", "hc064_p", "hc064_n", "hc071_t", "hc071_p", "hc071_n", "hc072_t", "hc072_p", "hc072_n", "hc073_t", "hc073_p", "hc073_n", "hc074_t", "hc074_p",
                              "hc074_n", "hc082_t", "hc082_p", "hc082_n", "hc083_t", "hc083_p", "hc083_n", "hc091_t", "hc091_p", "hc091_n", "hc092_t", "hc092_p", "hc092_n", "hc093_t", "hc093_p", "hc093_n", "hc094_t",
                              "hc094_p", "hc094_n", "hc095_t", "hc095_p", "hc095_n", "hc096_t", "hc096_p", "hc096_n", "ageyoch", "age", "yrsresid", "edyrs", "hourstot", "weeks", "wexptl", "hours1", "ppub_i",
                              "pitotal", "pilabour", "pi11", "pi12", "pi13", "pipension", "pi31", "pi311", "pi312", "pi32", "pi33", "pi331", "pi332", "pi411", "pi42", "pi421", "pi422",
                              "pi43", "pi44", "pi511", "pxitsc", "pxitax", "pxscont", "pxvcont", "pelabour", "momnum", "partnum", "dadnum", "wage1", "hwage1", "weeksft", "pxvcontp", "pxvconth", "pxvcontu",
                              "pxvconto")

# --- LCS Country Specific vars ----
lcs_country_specific_variables <- lcs_variables[stringr::str_sub(lcs_variables,-2,-1) == "_c"]



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

key_vars_household_lcs <- c("hid", "nhhmem", "hwgt", "hpopwgt", "hwgta","year","iso2", "dname", "currency")
key_vars_person_lcs <- c("hid", "pid", "relation", "pwgt", "ppopwgt", "pwgta","year","iso2", "dname", "currency")




# ---- Usethis ----
usethis::use_data(lis_variables,
                  lws_variables,
                  lcs_variables
                  overwrite = TRUE) 

usethis::use_data(lis_household_variables,
                  lis_person_variables,
                  lis_both_hp_variables,
                  lis_technical_variables,
                  lis_weight_variables,
                  lis_id_variables,
                  lis_categorical_variables,
                  lis_continuous_variables,
                  lis_country_specific_variables,
                  lis_income_variables,
                  lcs_household_variables,
                  lcs_person_variables,
                  lcs_both_hp_variables,
                  lcs_technical_variables,
                  lcs_weight_variables,
                  lcs_id_variables,
                  lcs_categorical_variables,
                  lcs_continuous_variables,
                  lcs_country_specific_variables,
                  lcs_consumption_variables,
                  lws_household_variables,
                  lws_person_variables,
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
                  key_vars_household_lcs,
                  key_vars_person_lcs, 
                  internal = TRUE, # to save on R/sysdata.rda
                  overwrite = TRUE)




