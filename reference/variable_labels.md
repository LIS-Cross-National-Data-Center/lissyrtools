# Inspect the Labels of LIS and LWS variables

Inspect the Labels of LIS and LWS variables

## Usage

``` r
variable_labels(vars = NULL)
```

## Arguments

- vars:

  A character vector containing LIS/LWS variables or the output list
  from lissyuse.

## Value

A character vector with the corresponding labels for the selected
variables.

## Examples

``` r
# 1) Without any argument:
variable_labels()
#>                                                           hid 
#>                                        "household identifier" 
#>                                                           pid 
#>                                           "person identifier" 
#>                                                           did 
#>                                  "unique country/year number" 
#>                                                         dname 
#>                                     "country/year identifier" 
#>                                                         cname 
#>                                                "country name" 
#>                                                          iso2 
#>                               "2-letter country abbreviation" 
#>                                                          iso3 
#>                               "3-letter country abbreviation" 
#>                                                          year 
#>                                              "reference year" 
#>                                                          wave 
#>                                                   "data wave" 
#>                                                       hpopwgt 
#>                                            "household weight" 
#>                                                       ppopwgt 
#>                                               "person weight" 
#>                                                          hwgt 
#>                                 "normalised household weight" 
#>                                                          pwgt 
#>                                    "normalised person weight" 
#>                                                         hwgta 
#>                                 "additional household weight" 
#>                                                         pwgta 
#>                                    "additional person weight" 
#>                                                      currency 
#>                                              "currency units" 
#>                                                      grossnet 
#>                                "gross/net income information" 
#>                                                      region_c 
#>                                    "region: country specific" 
#>                                                         rural 
#>                                                  "rural area" 
#>                                                       locsz_c 
#>                          "size of locality: country specific" 
#>                                                        area_c 
#>                              "type of area: country specific" 
#>                                                           own 
#>                                "main residence tenure status" 
#>                                                       dweltyp 
#>                                            "type of dwelling" 
#>                                                       farming 
#>                                            "farming activity" 
#>                                                        hhtype 
#>                        "household composition (discontinued)" 
#>                                                      hpartner 
#>              "household reference person living with partner" 
#>                                                        nhhmem 
#>                                 "number of household members" 
#>                                                      nhhmem65 
#>                     "number of household members 65 or older" 
#>                                                      nhhmem17 
#>                   "number of household members 17 or younger" 
#>                                                      nhhmem13 
#>                   "number of household members 13 or younger" 
#>                                                         nearn 
#>              "number of household members with labour income" 
#>                                                      relation 
#>                  "relationship to household reference person" 
#>                                                       partner 
#>                                         "living with partner" 
#>                                                       parents 
#>                                         "living with parents" 
#>                                                     nchildren 
#>                  "number of own children living in household" 
#>                                                       ageyoch 
#>               "age of youngest own child living in household" 
#>                                                           age 
#>                                                "age in years" 
#>                                                           sex 
#>                                                      "gender" 
#>                                                       marital 
#>                                              "marital status" 
#>                                                        immigr 
#>                                                   "immigrant" 
#>                                                       citizen 
#>                                                 "citizenship" 
#>                                                      ctrybrth 
#>                                            "country of birth" 
#>                                                      yrsresid 
#>                              "years since arrived in country" 
#>                                                      ethnic_c 
#>                                 "ethnicity: country specific" 
#>                                                      migrat_c 
#>                        "internal migration: country specific" 
#>                                                      immigr_c 
#>             "other immigration information: country specific" 
#>                                                      disabled 
#>                                    "indication of disability" 
#>                                                      health_c 
#>                  "subjective health status: country specific" 
#>                                                          educ 
#>                               "education (3-category recode)" 
#>                                                       educlev 
#>                           "highest completed education level" 
#>                                                        educ_c 
#>                   "highest education level: country specific" 
#>                                                        enroll 
#>                                       "enrolled in education" 
#>                                                         edyrs 
#>                                          "years of education" 
#>                                                    illiterate 
#>                                                  "illiterate" 
#>                                                       edmom_c 
#>                       "education of mother: country specific" 
#>                                                       eddad_c 
#>                       "education of father: country specific" 
#>                                                           emp 
#>                                                    "employed" 
#>                                                       emp_ilo 
#>                                   "employed (ILO definition)" 
#>                                                           lfs 
#>                                         "labour force status" 
#>                                                      informal 
#>                                           "informal activity" 
#>                                                      parleave 
#>                          "maternity/paternity/parental leave" 
#>                                                          fyft 
#>                                  "worked full-year full-time" 
#>                                                      hourstot 
#>                                   "total weekly hours worked" 
#>                                                         weeks 
#>                                         "annual weeks worked" 
#>                                                        secjob 
#>                                        "multiple jobs holder" 
#>                                                        wexptl 
#>                              "years of total work experience" 
#>                                                       status1 
#>                                        "status in employment" 
#>                                                         inda1 
#>                            "economic activity (3 categories)" 
#>                                                         indb1 
#>                            "economic activity (9 categories)" 
#>                                                         indc1 
#>             "economic activity (ISIC rev. 3.1: major groups)" 
#>                                                         indd1 
#>               "economic activity (ISIC rev. 4: major groups)" 
#>                                                        ind1_c 
#>                         "economic activity: country specific" 
#>                                                       public1 
#>                                               "public sector" 
#>                                                         occa1 
#>                                   "occupation (3 categories)" 
#>                                                         occb1 
#>                          "occupation (ISCO-88: major groups)" 
#>                                                        occ1_c 
#>                                "occupation: country specific" 
#>                                                         temp1 
#>                                        "temporary employment" 
#>                                                        ptime1 
#>                                        "part-time employment" 
#>                                                        hours1 
#>                             "weekly hours worked in main job" 
#>                                                          inum 
#>                                            "implicate number" 
#>                                                       hitotal 
#>                             "total current income, household" 
#>                                                           dhi 
#>                      "disposable household income, household" 
#>                                                        hvalgs 
#>                      "value of goods and services, household" 
#>                                                          dhci 
#>                 "disposable household cash income, household" 
#>                                                      hifactor 
#>                                    "factor income, household" 
#>                                                    hitransfer 
#>                                  "transfer income, household" 
#>                                                       hpublic 
#>                                 "public transfers, household" 
#>                                                        hpub_i 
#>                              "insurance transfers, household" 
#>                                                        ppub_i 
#>                                 "insurance transfers, person" 
#>                                                        hpub_u 
#>                              "universal transfers, household" 
#>                                                        hpub_a 
#>                             "assistance transfers, household" 
#>                                                       pitotal 
#>                             "total individual income, person" 
#>                                                         hcexp 
#>                          "consumption expenditure, household" 
#>                                                     hhouscost 
#>                                    "housing costs, household" 
#>                                                           dnw 
#>                                        "disposable net worth" 
#>                                                           anw 
#>                               "adjusted disposable net worth" 
#>                                                           inw 
#>                                        "integrated net worth" 
#>                                                           tnw 
#>                                             "total net worth" 
#>                                                      hilabour 
#>                                    "labour income, household" 
#>                                                      pilabour 
#>                                       "labour income, person" 
#>                                                          hi11 
#>                                      "wage income, household" 
#>                                                          pi11 
#>                                         "wage income, person" 
#>                                                          hi12 
#>                           "self-employment income, household" 
#>                                                          pi12 
#>                              "self-employment income, person" 
#>                                                         hi121 
#>                                      "farm income, household" 
#>                                                          hi13 
#>                                  "fringe benefits, household" 
#>                                                          pi13 
#>                                     "fringe benefits, person" 
#>                                                          hi14 
#>                                  "own consumption, household" 
#>                                                     hicapital 
#>                                   "capital income, household" 
#>                                                          hi21 
#>                           "interest and dividends, household" 
#>                                                          hi22 
#>                                    "rental income, household" 
#>                                                     hipension 
#>                                         "pensions, household" 
#>                                                     pipension 
#>                                            "pensions, person" 
#>                                                          hi31 
#>                 "public non-contributory pensions, household" 
#>                                                          pi31 
#>                    "public non-contributory pensions, person" 
#>                                                         hi311 
#>                               "universal pensions, household" 
#>                                                         pi311 
#>                                  "universal pensions, person" 
#>                                                         hi312 
#>                              "assistance pensions, household" 
#>                                                         pi312 
#>                                 "assistance pensions, person" 
#>                                                          hi32 
#>                     "public contributory pensions, household" 
#>                                                          pi32 
#>                        "public contributory pensions, person" 
#>                                                          hi33 
#>                                 "private pensions, household" 
#>                                                          pi33 
#>                                    "private pensions, person" 
#>                                                         hi331 
#>                            "occupational pensions, household" 
#>                                                         pi331 
#>                               "occupational pensions, person" 
#>                                                         hi332 
#>                              "individual pensions, household" 
#>                                                         pi332 
#>                                 "individual pensions, person" 
#>                                                      hipubsoc 
#>          "public social benefits (excl. pensions), household" 
#>                                                          hi41 
#>                                  "family benefits, household" 
#>                                                         hi411 
#>             "maternity/paternity and parental leave payments" 
#>                                                         pi411 
#>             "maternity/paternity and parental leave payments" 
#>                                                         hi412 
#>                                  "child allowance, household" 
#>                                                          hi42 
#>                            "unemployment benefits, household" 
#>                                                          pi42 
#>                               "unemployment benefits, person" 
#>                                                         hi421 
#>                           "unemployment insurance, household" 
#>                                                         pi421 
#>                              "unemployment insurance, person" 
#>                                                         hi422 
#>                          "unemployment assistance, household" 
#>                                                         pi422 
#>                             "unemployment assistance, person" 
#>                                                          hi43 
#>                     "sickness/temporary work injury payments" 
#>                                                          pi43 
#>                     "sickness/temporary work injury payments" 
#>                                                          hi44 
#>                   "disability/permanent work injury benefits" 
#>                                                          pi44 
#>                   "disability/permanent work injury benefits" 
#>                                                          hi45 
#>                              "general asssistance, household" 
#>                                                          hi46 
#>                                 "housing benefits, household" 
#>                                                          hi47 
#>                          "public in-kind benefits, household" 
#>                                                         hi471 
#>                                    "food benefits, household" 
#>                                                     hiprivate 
#>                                "private transfers, household" 
#>                                                          hi51 
#>         "cash transfers from private institutions, household" 
#>                                                         hi511 
#>                                     "scholarships, household" 
#>                                                         pi511 
#>                                        "scholarships, person" 
#>                                                          hi52 
#>                   "inter-household cash transfers, household" 
#>                                                         hi521 
#>                        "alimony and child support, household" 
#>                                                         hi522 
#>                                      "remittances, household" 
#>                                                          hi53 
#>                        "private in-kind transfers, household" 
#>                                                         hi531 
#>      "in-kind transfers from private institutions, household" 
#>                                                         hi532 
#>          "in-kind transfers from other households, household" 
#>                                                        hxitsc 
#>                   "income taxes and contributions, household" 
#>                                                        pxitsc 
#>                      "income taxes and contributions, person" 
#>                                                        hxitax 
#>                                     "income taxes, household" 
#>                                                        pxitax 
#>                                        "income taxes, person" 
#>                                                       hxscont 
#>                    "social security contributions, household" 
#>                                                       pxscont 
#>                       "social security contributions, person" 
#>                                                        hxotax 
#>                               "other direct taxes, household" 
#>                                                        hxptax 
#>                                   "property taxes, household" 
#>                                                       hxvcont 
#>                          "voluntary contributions, household" 
#>                                                       pxvcont 
#>                             "voluntary contributions, person" 
#>                                                         hxiht 
#>                   "inter-household transfers paid, household" 
#>                                                        hxalim 
#>                   "alimony and child support paid, household" 
#>                                                       hxremit 
#>                                 "remittances paid, household" 
#>                                                        hxmort 
#>                                       "mortgage installments" 
#>                                                        hxintm 
#>                                     "mortgage interests paid" 
#>                                                      helabour 
#>                      "extraordinary labour income, household" 
#>                                                      pelabour 
#>                         "extraordinary labour income, person" 
#>                                                       hecgain 
#>                                    "capital gains, household" 
#>                                                     heinherit 
#>                             "inheritance received, household" 
#>                                                         heoth 
#>                       "other extraordinary income, household" 
#>                                                        hrenti 
#>                                     "imputed rent, household" 
#>                                                           hc1 
#>                 "food and non-alcoholic beverages, household" 
#>                                                           hc2 
#>                              "alcohol and tobacco, household" 
#>                                                           hc3 
#>                            "clothing and footwear, household" 
#>                                                           hc4 
#>                        "actual rent and utilities, household" 
#>                                                          hc41 
#>                                      "actual rent, household" 
#>                                                           hc5 
#>                                "housing equipment, household" 
#>                                                           hc6 
#>                                           "health, household" 
#>                                                           hc7 
#>                                        "transport, household" 
#>                                                           hc8 
#>                                    "communication, household" 
#>                                                           hc9 
#>                           "recreation and culture, household" 
#>                                                          hc10 
#>                                        "education, household" 
#>                                                          hc11 
#>                           "restaurants and hotels, household" 
#>                                                          hc12 
#>                 "miscellaneous goods and services, household" 
#>                                                        hxloan 
#>                                "installments for other loans" 
#>                                                        hxintl 
#>                               "interests paid on other loans" 
#>                                                           cir 
#>                         "expects to receive inheritance/gift" 
#>                                                           cia 
#>                         "amount of expected inheritance/gift" 
#>                                                           cig 
#>                            "expects to give inheritance/gift" 
#>                                                           chc 
#>                          "home equity line of credit (dummy)" 
#>                                                           cha 
#>                        "amount of home equity line of credit" 
#>                                                           cnc 
#>                     "non-home equity lines of credit (dummy)" 
#>                                                           cna 
#>                   "amount of non-home equity lines of credit" 
#>                                                           ppr 
#>                       "purchase price of principal residence" 
#>                                                           ppy 
#>                     "year of purchase of principal residence" 
#>                                                           pir 
#>                           "inheritance/gift received (dummy)" 
#>                                                          pia1 
#>                       "amount of inheritance/gift received 1" 
#>                                                          pia2 
#>                       "amount of inheritance/gift received 2" 
#>                                                          pia3 
#>                       "amount of inheritance/gift received 3" 
#>                                                          pia4 
#>                       "amount of inheritance/gift received 4" 
#>                                                          piy1 
#>                            "year inheritance/gift received 1" 
#>                                                          piy2 
#>                            "year inheritance/gift received 2" 
#>                                                          piy3 
#>                            "year inheritance/gift received 3" 
#>                                                          piy4 
#>                            "year inheritance/gift received 4" 
#>                                                          pit1 
#>                         "type of inheritance/gift received 1" 
#>                                                          pit2 
#>                         "type of inheritance/gift received 2" 
#>                                                          pit3 
#>                         "type of inheritance/gift received 3" 
#>                                                          pit4 
#>                         "type of inheritance/gift received 4" 
#>                                                          piw1 
#>                       "from whom inheritance/gift received 1" 
#>                                                          piw2 
#>                       "from whom inheritance/gift received 2" 
#>                                                          piw3 
#>                       "from whom inheritance/gift received 3" 
#>                                                          piw4 
#>                       "from whom inheritance/gift received 4" 
#>                                                          pim1 
#>            "household member who received inheritance/gift 1" 
#>                                                          pim2 
#>            "household member who received inheritance/gift 2" 
#>                                                          pim3 
#>            "household member who received inheritance/gift 3" 
#>                                                          pim4 
#>            "household member who received inheritance/gift 4" 
#>                                                          ssyc 
#>            "years of contributions to social security system" 
#>                                                        bus1_c 
#>                       "business indicator/type of business 1" 
#>                                                        bus2_c 
#>                       "business indicator/type of business 2" 
#>                                                        bus3_c 
#>                       "business indicator/type of business 3" 
#>                                                          basb 
#>                                           "savings behaviour" 
#>                                                         basp1 
#>                              "savings purpose: home purchase" 
#>                                                         basp2 
#>                      "savings purpose: other major purchases" 
#>                                                         basp3 
#> "savings purpose: financial investments (including business)" 
#>                                                         basp4 
#>                         "savings purpose: old-age provisions" 
#>                                                         basp5 
#>                               "savings purpose: paying debts" 
#>                                                         basp6 
#>                          "savings purpose: unexpected events" 
#>                                                         basp7 
#>                      "savings purpose: recreation and travel" 
#>                                                         basp8 
#>                                  "savings purpose: education" 
#>                                                         basp9 
#>                                      "savings purpose: other" 
#>                                                       bafr1_c 
#>                                     "financial risk taking 1" 
#>                                                       bafr2_c 
#>                                     "financial risk taking 2" 
#>                                                       bafr3_c 
#>                                     "financial risk taking 3" 
#>                                                       bafp1_c 
#>                                        "financial planning 1" 
#>                                                       bafp2_c 
#>                                        "financial planning 2" 
#>                                                       bafl1_c 
#>                                        "financial literacy 1" 
#>                                                       bafl2_c 
#>                                        "financial literacy 2" 
#>                                                       bafl3_c 
#>                                        "financial literacy 3" 
#>                                                       bafl4_c 
#>                                        "financial literacy 4" 
#>                                                       bafi1_c 
#>                                     "financial information 1" 
#>                                                       bafi2_c 
#>                                     "financial information 2" 
#>                                                       bafi3_c 
#>                                     "financial information 3" 
#>                                                       bafi4_c 
#>                                     "financial information 4" 
#>                                                       bafi5_c 
#>                                     "financial information 5" 
#>                                                          bopc 
#>                          "possession of credit cards (dummy)" 
#>                                                        bonc_c 
#>                                      "number of credit cards" 
#>                                                          boue 
#>                  "amount needed to cover unexpected expenses" 
#>                                                          boea 
#>              "ability to get emergency financial aid (dummy)" 
#>                                                          bolc 
#>                                  "loan consolidation (dummy)" 
#>                                                       bocc1_c 
#>                              "objective credit constraints 1" 
#>                                                       bocc2_c 
#>                              "objective credit constraints 2" 
#>                                                        bocs_c 
#>                               "subjective credit constraints" 
#>                                                       bocd1_c 
#>                             "constraints in debt repayment 1" 
#>                                                       bocd2_c 
#>                             "constraints in debt repayment 2" 
#>                                                        boef_c 
#>                       "expectations about household finances" 
#>                                                       boee1_c 
#>                            "expectations about the economy 1" 
#>                                                       boee2_c 
#>                            "expectations about the economy 2" 
#>                                                            ha 
#>                                                "total assets" 
#>                                                           han 
#>                                        "non-financial assets" 
#>                                                          hanr 
#>                                                 "real estate" 
#>                                                         hanrp 
#>                                         "principal residence" 
#>                                                         hanro 
#>                                           "other real estate" 
#>                                                          hann 
#>                                          "non-housing assets" 
#>                                                         hannb 
#>                                             "business equity" 
#>                                                         hannc 
#>                                              "consumer goods" 
#>                                                        hanncv 
#>                                                    "vehicles" 
#>                                                        hanncd 
#>                                "other durables and valuables" 
#>                                                         hanno 
#>                                  "other non-financial assets" 
#>                                                           haf 
#>                       "financial assets (excluding pensions)" 
#>                                                          hafc 
#>                                   "deposit accounts and cash" 
#>                                                          hafi 
#>                                       "financial investments" 
#>                                                         hafib 
#>                             "bonds and other debt securities" 
#>                                                         hafis 
#>                                     "stocks and other equity" 
#>                                                         hafii 
#>                "investment funds and alternative investments" 
#>                                                          hafo 
#>                          "other non-pension financial assets" 
#>                                                           has 
#>                  "pension assets and other long-term savings" 
#>                                                           pas 
#>          "pension assets and other long-term savings, person" 
#>                                                          hasi 
#>            "life insurance and voluntary individual pensions" 
#>                                                          pasi 
#>    "life insurance and voluntary individual pensions, person" 
#>                                                         hasil 
#>                                     "life insurance accounts" 
#>                                                         pasil 
#>                             "life insurance accounts, person" 
#>                                                         hasip 
#>                       "individual voluntary pension accounts" 
#>                                                         pasip 
#>               "individual voluntary pension accounts, person" 
#>                                                          haso 
#>                                       "occupational pensions" 
#>                                                          paso 
#>                               "occupational pensions, person" 
#>                                                        hasodb 
#>                          "occupational pensions (DB schemes)" 
#>                                                        pasodb 
#>                  "occupational pensions (DB schemes), person" 
#>                                                        hasodc 
#>                          "occupational pensions (DC schemes)" 
#>                                                        pasodc 
#>                  "occupational pensions (DC schemes), person" 
#>                                                          hass 
#>                        "social security pension entitlements" 
#>                                                          pass 
#>                "social security pension entitlements, person" 
#>                                                        hassdb 
#>                                "social security (DB schemes)" 
#>                                                        passdb 
#>                        "social security (DB schemes), person" 
#>                                                        hassdc 
#>                                "social security (DC schemes)" 
#>                                                        passdc 
#>                        "social security (DC schemes), person" 
#>                                                            hl 
#>                                           "total liabilities" 
#>                                                           hlr 
#>                                     "real estate liabilities" 
#>                                                          hlrp 
#>                                   "principal residence loans" 
#>                                                          hlro 
#>                                     "other real estate loans" 
#>                                                           hln 
#>                                     "non-housing liabilities" 
#>                                                          hlni 
#>                                            "investment loans" 
#>                                                          hlnc 
#>                                        "consumer goods loans" 
#>                                                         hlncv 
#>                                               "vehicle loans" 
#>                                                         hlncd 
#>                       "other loans for goods and consumption" 
#>                                                          hlne 
#>                                             "education loans" 
#>                                                          hlno 
#>                               "other non-housing liabilities" 
#>                                                          hlsr 
#>                  "institutional loans secured by real estate" 
#>                                                         hlsrp 
#>                              "secured by principal residence" 
#>                                                         hlsro 
#>                                "secured by other real estate" 
#>                                                          hlsn 
#>                            "loans not secured by real estate" 
#>                                                         hlsng 
#>                              "guaranteed institutional loans" 
#>                                                         hlsnn 
#>                          "non-guaranteed institutional loans" 
#>                                                         hlsni 
#>                          "informal (non-institutional) loans" 
#>                                                        nrooms 
#>                  "number of rooms available to the household" 
#>                                                        typehh 
#>                                              "household type" 
#>                                                       nhhmem6 
#>                    "number of household members 6 or younger" 
#>                                                     oneparent 
#>                                                 "lone parent" 
#>                                                      depchild 
#>                                             "dependent child" 
#>                                                        momnum 
#>                                       "pointer to the mother" 
#>                                                       partnum 
#>                                      "pointer to the partner" 
#>                                                        dadnum 
#>                                       "pointer to the father" 
#>                                                         occc1 
#>                          "occupation (ISCO-08: major groups)" 
#>                                                         wage1 
#>                                               "monthly  wage" 
#>                                                        hwage1 
#>                                                 "hourly wage" 
#>                                                       weeksft 
#>                               "annual weeks worked full-time" 
#>                                                         hafct 
#>                               "transaction accounts and cash" 
#>                                                         hafcs 
#>                                             "saving accounts" 
#>                                                        hafiss 
#>                                      "publicly traded stocks" 
#>                                                        hafiso 
#>                            "unlisted shares and other equity" 
#>                                                         hafom 
#>                                     "money owed to household" 

if (FALSE) { # \dontrun{
# 2) Using with the outputed list from lissyuse:
lis_datasets <- lissyuse(data = c("uk"), vars = c("hpub_i","hpub_u", "hi42", "hi421", "hi422", "hi43"), from = 2016)
variable_labels(vars = lis_datasets)
} # }

# 3) Using a character vector with LIS/LWS variables:
variable_labels(vars = c("fyft", "basb", "hxremit", "bafi1_c", "pasodc"))
#>                                         fyft 
#>                 "worked full-year full-time" 
#>                                         basb 
#>                          "savings behaviour" 
#>                                      hxremit 
#>                "remittances paid, household" 
#>                                      bafi1_c 
#>                    "financial information 1" 
#>                                       pasodc 
#> "occupational pensions (DC schemes), person" 
```
