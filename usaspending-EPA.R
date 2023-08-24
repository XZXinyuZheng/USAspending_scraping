library(httr)
library(jsonlite)
library(tidyverse)

# Account data ------------------------------------------------------------

url_account <- "https://api.usaspending.gov/api/v2/download/accounts/"

input_account_level <- "treasury_account"

input_agency_code <- c("61")

input_year <- as.character(2018:2023)

input_num_quarters <- "4"

account_download <- function(account_level, agency_code, year, num_quarters){
  
  if (length(account_level) > 1 | !(account_level %in% c("treasury_account", "federal_account"))) {
    
    pring('ERROR:The length of input_account_level can only be either "treasury_account" or "federal_account"')
    
    break
  } 
  
  else if (length(num_quarters) > 1 | !(num_quarters %in% c("1", "2", "3", "4"))) {
    
    print('ERROR: The length of input_num_quarters can only be "1", "2", "3", or "4"')
    
    break
  } 
  
  else {
    
    postbody <- 
      list(
        account_level = account_level,
        filters =
          list(
            budget_function = "all",
            agency = agency_code,
            submission_types = 
              list(
                "account_balances",
                "object_class_program_activity",
                "award_financial"),
            fy = year,
            quarter = num_quarters),
        file_formate = "csv")
    
    respond <- 
      POST(
        url_account,
        body = postbody,
        encode = "json") %>% 
      content(as = "text") %>% 
      fromJSON()
    
    # assign("respond", respond, envir = globalenv())
    
    write_file <-
      GET(respond$file_url)
    
    while (write_file$status_code != 200) {
      
      print("Downloading in progress")
      
      Sys.sleep(60)
      
      write_file <-
        GET(respond$file_url)
    }
    
    zip_path <- paste0("EPA/", year, ".zip")
    
    GET(
      respond$file_url,
      write_disk(
        zip_path,
        overwrite = TRUE))
    
    unzip(
      zipfile = zip_path,
      exdir = "EPA",
      overwrite = TRUE)
    
    unlink(zip_path)
  }
}

# account_download("treasury_account", "61", "2023", "4")

inputs <- 
  list(
    account_level =       
      rep(
        input_account_level,
        each = length(input_year) * length(input_agency_code)),
    agency_code = 
      rep(
        input_agency_code, 
        each = length(input_year)),
    year = 
      rep(
        input_year, 
        times = length(input_agency_code)),
    num_quarters = 
      rep(
        input_num_quarters,
        each = length(input_year) * length(input_agency_code)))

pmap(
  .l = inputs,
  .f = account_download)


# Award -------------------------------------------------------------------

url_award <- "https://api.usaspending.gov/api/v2/bulk_download/awards/"

input_start_date <- 
  c(
    # "1999-10-01", 
    # "2000-10-01", 
    # "2001-10-01",
    # "2002-10-01",
    # "2003-10-01",
    # "2004-10-01",
    # "2005-10-01",
    # "2006-10-01",
    # "2007-10-01",
    # "2008-10-01",
    # "2009-10-01",
    # "2010-10-01",
    # "2011-10-01",
    # "2012-10-01",
    # "2013-10-01",
    # "2014-10-01",
    # "2015-10-01",
    # "2016-10-01",
    # "2017-10-01",
    # "2018-10-01",
    # "2019-10-01",
    # "2020-10-01",
    "2021-10-01",
    "2022-10-01")

input_end_date <- 
  c(
    # "2000-09-30", 
    # "2001-09-30", 
    # "2002-09-30",
    # "2003-09-30",
    # "2004-09-30",
    # "2005-09-30",
    # "2006-09-30",
    # "2007-09-30",
    # "2008-09-30",
    # "2009-09-30",
    # "2010-09-30",
    # "2011-09-30",
    # "2012-09-30",
    # "2013-09-30",
    # "2014-09-30",
    # "2015-09-30",
    # "2016-09-30",
    # "2017-09-30",
    # "2018-09-30",
    # "2019-09-30",
    # "2020-09-30",
    # "2021-09-30",
    "2022-09-30",
    "2023-06-30")

input_agency_type <- "funding"

input_agency_name <- c("Environmental Protection Agency")

award_download <- function(agency_type, agency_name, start_date, end_date){
  
  postbody <- 
    list(
      filters =
        list(
          prime_award_types = 
            list("A","B","C","D","IDV_A","IDV_B","IDV_B_A","IDV_B_B","IDV_B_C","IDV_C","IDV_D","IDV_E","02","03","04","05","10","06","07","08","09","11"),
          sub_award_types = 
            list("procurement","grant"),
          date_type = "action_date",
          date_range = 
            list(
              start_date = start_date,
              end_date = end_date),
          agencies = 
            list(
              list(
                type = agency_type,
                tier = "toptier",
                name = agency_name))),
      file_formate = "csv")
  
  respond <- 
    POST(
      url_award,
      body = postbody,
      encode = "json") %>% 
    content(as = "text") %>% 
    fromJSON()
  
  write_file <-
    GET(respond$file_url)
  
  while (write_file$status_code != 200) {
    
    print("Downloading in progress")
    
    Sys.sleep(60)
    
    write_file <-
      GET(respond$file_url)
  }
  
  zip_path <- paste0("EPA/AwardData_Funding/", start_date, ".zip")
  
  GET(
    respond$file_url,
    write_disk(
      zip_path,
      overwrite = TRUE))
  
  unzip(
    zipfile = zip_path,
    exdir = "EPA/AwardData_Funding",
    overwrite = TRUE)
  
  unlink(zip_path)
  
}

# award_download(
#   agency_type = "awarding", 
#   agency_name = "Environmental Protection Agency", 
#   start_date = "1999-10-01", 
#   end_date = "2000-09-30")

inputs <- 
  list(
    agency_type = 
      rep(
        input_agency_type,
        each = length(input_agency_name)*length(input_start_date)),
    agency_name = 
      rep(
        input_agency_name,
        each = length(input_start_date)),
    start_date = 
      rep(
        input_start_date, 
        times = length(input_agency_name)),
    end_date = 
      rep(
        input_end_date,
        times = length(input_agency_name)))

pmap(
  .l = inputs,
  .f = award_download)


# troubleshooting ---------------------------------------------------------

fix <- read_csv("EPA/AwardData_Awarding/All_Assistance_Subawards_2023-07-10_H10M09S57_1_origin.csv")

fix$subaward_description <- 
  fix$subaward_description %>% 
  str_replace_all('"', "'")

# i <- 1
# na_columns_char <-c()
# na_columns_nochar <- c()
# 
# while (i <= 105) {
#   
#   if (fix[, i] %>% 
#       is.na() %>% 
#       mean() > 0) {
#     
#     if (fix[, 35] %>% is.character()) {
#       na_columns_char <- c(na_columns_char, names(fix)[i])
#     } else {
#       na_columns_nochar <- c(na_columns_nochar, names(fix)[i])
#     }
#   }
#   
#   i <- i + 1
# } 

# na_columns %>% 
#   map(
#     ~ if (pull(fix, .x) %>% is.character()) {
#       
#       fix$.x <-  
#         pull(fix, .x) %>% 
#         replace_na("")
#     }
#     else {
#       fix$.x <- 
#         pull(fix, .x) %>% 
#         as.character() %>% 
#         replace_na("") %>% 
#         as.numeric()
#     })

write_csv(fix, "EPA/AwardData_Awarding/All_Assistance_Subawards_2023-07-10_H10M09S57_1.csv", quote = "all")

