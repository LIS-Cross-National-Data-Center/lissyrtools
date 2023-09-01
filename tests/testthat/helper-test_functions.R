# helper-test_functions.R

testdata_path <- function(){

  if(stringr::str_detect(here::here(),
                         pattern = "lissyrtools")){

    return(here::here("tests", "testdata"))

  }else{

    return(here::here("lissyrtools", "tests", "testdata"))

  }

}
