

# setup -------------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

# Account Breakdown by Program Activity & Object Class (File B)

## disaster_emergency_fund_name: INFRASTRUCTURE INVESTMENT AND JOB ACT
## program_activity_name: INFLATION REDUCTION ACT

# Account Breakdown by Award* (File C)

## disaster_emergency_fund_name: INFRASTRUCTURE INVESTMENT AND JOB ACT

# federal account exp w/ DOE ----------------------------------------------------

url_DOEaccount <- "https://api.usaspending.gov/api/v2/download/accounts/"

postbody_DOEaccount <- 
  list(
    account_level = "federal_account",
    filters =
      list(
        budget_function = "all",
        agency = "15",
        submission_types =  list(
          "account_balances",
          "object_class_program_activity",
          "award_financial"),
        fy = "2022",
        period = "12"),
    file_formate = "csv")

# '{"account_level":"federal_account","filters":{"budget_function":"all","agency":"78","submission_types":["object_class_program_activity"],"fy":"2022","period":12},"file_format":"csv"}'

respond_DOEaccount <- 
  POST(
    url_DOEaccount,
    body = postbody_DOEaccount,
    encode = "json") %>% 
  content(as = "text") %>% 
  fromJSON()

status <- 
  GET(
    respond_DOEaccount$status_url) # The status here is always 200

write_file <- GET(
  respond_DOEaccount$file_url,
  write_disk(
    "IRA_BIL/IRA_BIL_DOE.zip",
    overwrite = TRUE))

unzip(
  zipfile = "IRA_BIL/IRA_BIL_DOE.zip",
  exdir = "IRA_BIL",
  overwrite = TRUE)

unlink("IRA_BIL/IRA_BIL_DOE.zip")

DOEaccount <- read_csv("IRA_BIL/FY2022P01-P12_All_FA_AccountBreakdownByPA-OC_2023-07-02_H16M14S31_1.csv")

DOEaccount %>% 
  filter(
    program_activity_name %in% 
      c(
        "INFLATION REDUCTION ACT", 
        "INFRASTRUCTURE INVESTMENT AND JOB ACT"))

# AUTOMATION --------------------------------------------------------------

## Create a blank fileB to store filtered data ------------------------------

if(
  
  # Check if IRA_BIL_FileB_base.csv file exist
  
  file.exists("IRA_BIL/IRA_BIL_FileB_base.csv")){
  
  # If yes, load in the base fileB
  
  base_fileB <- read_csv("IRA_BIL/IRA_BIL_FileB_base.csv")
  
} else {
  
  # If no, create the base fileB
  
  base_fileB_matric <- matrix(ncol = 48, nrow = 0)
  
  colnames(base_fileB_matric) <- 
    c(
      "owning_agency_name",
      "reporting_agency_name",
      "submission_period",
      "agency_identifier_name",
      "budget_function",
      "budget_subfunction",
      "federal_account_symbol",
      "federal_account_name",
      "program_activity_code", 
      "program_activity_name",
      "object_class_code",
      "object_class_name",
      "direct_or_reimbursable_funding_source",
      "disaster_emergency_fund_code",
      "disaster_emergency_fund_name",
      "obligations_incurred",
      "obligations_undelivered_orders_unpaid_total",
      "obligations_undelivered_orders_unpaid_total_FYB",
      "USSGL480100_undelivered_orders_obligations_unpaid",
      "USSGL480100_undelivered_orders_obligations_unpaid_FYB",
      "USSGL488100_upward_adj_prior_year_undeliv_orders_oblig_unpaid",
      "obligations_delivered_orders_unpaid_total",
      "obligations_delivered_orders_unpaid_total_FYB",
      "USSGL490100_delivered_orders_obligations_unpaid",
      "USSGL490100_delivered_orders_obligations_unpaid_FYB",
      "USSGL498100_upward_adj_of_prior_year_deliv_orders_oblig_unpaid",
      "gross_outlay_amount_FYB_to_period_end",
      "gross_outlay_amount_FYB",
      "gross_outlays_undelivered_orders_prepaid_total",
      "gross_outlays_undelivered_orders_prepaid_total_FYB",
      "USSGL480200_undelivered_orders_obligations_prepaid_advanced",
      "USSGL480200_undelivered_orders_obligations_prepaid_advanced_FYB",
      "USSGL488200_upward_adj_prior_year_undeliv_orders_oblig_prepaid",
      "gross_outlays_delivered_orders_paid_total",
      "gross_outlays_delivered_orders_paid_total_FYB",
      "USSGL490200_delivered_orders_obligations_paid",
      "USSGL490800_authority_outlayed_not_yet_disbursed",
      "USSGL490800_authority_outlayed_not_yet_disbursed_FYB",
      "USSGL498200_upward_adj_of_prior_year_deliv_orders_oblig_paid",
      "deobligations_or_recoveries_or_refunds_from_prior_year",
      "USSGL487100_downward_adj_prior_year_unpaid_undeliv_orders_oblig",
      "USSGL497100_downward_adj_prior_year_unpaid_deliv_orders_oblig",
      "USSGL487200_downward_adj_prior_year_prepaid_undeliv_order_oblig",
      "USSGL497200_downward_adj_of_prior_year_paid_deliv_orders_oblig",
      "USSGL483100_undelivered_orders_obligations_transferred_unpaid",
      "USSGL493100_delivered_orders_obligations_transferred_unpaid",
      "USSGL483200_undeliv_orders_oblig_transferred_prepaid_advanced", 
      "last_modified_date")
  
  base_fileB <- 
    as_tibble(
      base_fileB_matric,
      .name_repair = "minimal")
  
  rm(base_fileB_matric)
  
  base_fileB %>% 
    write_csv(
      "IRA_BIL/IRA_BIL_FileB_base.csv")
}

## Create a blank fileC to store filtered data ------------------------------

if(
  
  # Check if IRA_BIL_FileC_base.csv file exist
  
  file.exists("IRA_BIL/IRA_BIL_FileC_base.csv")){
  
  # If yes, load in the base fileC
  
  base_fileC <- read_csv("IRA_BIL/IRA_BIL_FileC_base.csv")
  
} else {
  
  # If no, create the base fileC
  
  base_fileC_matric <- matrix(ncol = 77, nrow = 0)
  
  colnames(base_fileC_matric) <- 
    c(
      "owning_agency_name",                                             
      "reporting_agency_name",                                          
      "submission_period",                                            
      "federal_account_symbol",                                         
      "federal_account_name",                                           
      "agency_identifier_name",                                         
      "budget_function",                                           
      "budget_subfunction",                                             
      "program_activity_code",                                          
      "program_activity_name",                                          
      "object_class_code",                                              
      "object_class_name",                                              
      "direct_or_reimbursable_funding_source",                          
      "disaster_emergency_fund_code",                                   
      "disaster_emergency_fund_name",                                   
      "transaction_obligated_amount",                                   
      "gross_outlay_amount_FYB_to_period_end",                          
      "USSGL487200_downward_adj_prior_year_prepaid_undeliv_order_oblig",
      "USSGL497200_downward_adj_of_prior_year_paid_deliv_orders_oblig", 
      "award_unique_key",                                               
      "award_id_piid",                                                  
      "parent_award_id_piid",                                           
      "award_id_fain",                                                  
      "award_id_uri",                                                   
      "award_base_action_date",                                         
      "award_base_action_date_fiscal_year",                             
      "award_latest_action_date",                                       
      "award_latest_action_date_fiscal_year",                           
      "period_of_performance_start_date",                               
      "period_of_performance_current_end_date",                         
      "ordering_period_end_date",                                       
      "award_type_code",                                                
      "award_type",                                                     
      "idv_type_code",                                                  
      "idv_type",                                                       
      "prime_award_base_transaction_description",                       
      "awarding_agency_code",                                           
      "awarding_agency_name",                                           
      "awarding_subagency_code",                                        
      "awarding_subagency_name",                                        
      "awarding_office_code",                                           
      "awarding_office_name",                                           
      "funding_agency_code",                                            
      "funding_agency_name",                                            
      "funding_sub_agency_code",                                        
      "funding_sub_agency_name",                                        
      "funding_office_code",                                            
      "funding_office_name",                                            
      "recipient_uei",                                                  
      "recipient_duns",                                                 
      "recipient_name",                                                 
      "recipient_name_raw",                                             
      "recipient_parent_uei",                                           
      "recipient_parent_duns",                                          
      "recipient_parent_name",                                          
      "recipient_parent_name_raw",                                      
      "recipient_country",                                              
      "recipient_state",                                                
      "recipient_county",                                               
      "recipient_city",                                                 
      "recipient_congressional_district",                               
      "recipient_zip_code",                                             
      "primary_place_of_performance_country",                           
      "primary_place_of_performance_state",                             
      "primary_place_of_performance_county",                            
      "primary_place_of_performance_congressional_district",            
      "primary_place_of_performance_zip_code",                          
      "cfda_number",                                                    
      "cfda_title",                                                     
      "product_or_service_code",                                        
      "product_or_service_code_description",                            
      "naics_code",                                                     
      "naics_description",                                              
      "national_interest_action_code",                                  
      "national_interest_action",                                       
      "usaspending_permalink",                                          
      "last_modified_date")
  
  base_fileC <- 
    as_tibble(
      base_fileC_matric,
      .name_repair = "minimal")
  
  rm(base_fileC_matric)
  
  base_fileC %>% 
    write_csv(
      "IRA_BIL/IRA_BIL_FileC_base.csv")
}

## Retrieve a list of agency codes -----------------------------------------

agency_info <- POST(
  "https://api.usaspending.gov/api/v2/bulk_download/list_agencies/",
  body = 
    list(
      type = "account_agencies"),
  encode = "json") %>% 
  content(as = "text") %>% 
  fromJSON()

agency_code_list <- 
  c(agency_info$agencies$cfo_agencies$toptier_agency_id,
    agency_info$agencies$other_agencies$toptier_agency_id) %>% 
  as.character()

## A loop to download Account Data by agencies (VERSION 1) ---------------------------

url <- "https://api.usaspending.gov/api/v2/download/accounts/"

IRA_BIL_download <- function(agency_code, year, months){
  
  postbody <- 
    list(
      account_level = "federal_account",
      filters =
        list(
          budget_function = "all",
          agency = agency_code,
          submission_types = list("object_class_program_activity"),
          fy = year,
          period = months),
      file_formate = "csv")
  
  respond <- 
    POST(
      url,
      body = postbody,
      encode = "json") %>% 
    content(as = "text") %>% 
    fromJSON()
  
  zip_path <- paste0("IRA_BIL/raw/", agency_code, "_", year, ".zip")
  
  GET(
    respond$file_url,
    write_disk(
      zip_path,
      overwrite = TRUE))
  
  unzip(
    zipfile = zip_path,
    exdir = "IRA_BIL/raw",
    overwrite = TRUE)
  
  unlink(zip_path)
  
  Sys.sleep(2.5)
  
}

IRA_BIL_download("15", "2023", "6")

inputs <- 
  list(
    agency_code = agency_code_list,
    year = rep(
      "2023", 
      times = length(agency_code_list)),
    months = rep(
      "6",
      times = length(agency_code_list)))

pmap(
  .l = inputs,
  .f = IRA_BIL_download)

# Filter to IRA and BIL

list.files(
  "IRA_BIL/raw",
  full.names = TRUE) %>%
  # list("IRA_BIL/raw/FY2022P01-P12_All_FA_AccountBreakdownByPA-OC_2023-07-02_H16M14S31_1.csv") %>% 
  map(
    ~ read_csv(.x) %>% 
      filter(
        str_detect(
          program_activity_name, 
          "INFLATION REDUCTION ACT|INFRASTRUCTURE INVESTMENT AND JOB ACT|INFRASTRUCTURE INVESTMENT AND JOBS ACT") |
          disaster_emergency_fund_name == "Infrastructure Investment and Jobs Act") %>% 
      rbind(base_file) %>% 
      assign(
        "base_file", 
        .,
        envir = globalenv()))

write_csv(base_file, "IRA_BIL/IRA_BIL_FileB.csv")

## A loop to download Account Data by agencies (VERSION 2)-----------------------------------

url_account <- "https://api.usaspending.gov/api/v2/download/accounts/"

ira_bil_download <- function(account_level, agency_code, year, num_quarters){
  
  if (length(account_level) > 1 | 
      !(account_level %in% c("treasury_account", "federal_account"))) {
    
    pring('ERROR:The length of input_account_level can only be either "treasury_account" or "federal_account"')
    
    break
  } 
  
  else if (length(num_quarters) > 1 | 
           !(num_quarters %in% c("1", "2", "3", "4"))) {
    
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
                # "account_balances",
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
      
      Sys.sleep(300) # Change as needed
      
      write_file <-
        GET(respond$file_url)
    }
    
    # return(write_file$status_code)
    
    zip_path <- paste0("IRA_BIL/raw/", agency_code, year, ".zip")
    
    GET(
      respond$file_url,
      write_disk(
        zip_path,
        overwrite = TRUE))
    
    unzip(
      zipfile = zip_path,
      exdir = "IRA_BIL/raw",
      overwrite = TRUE)
    
    unlink(zip_path)
    
    # File C: Comment out the following codes because that some original files have
    # special symbols to mess the table formatting so that the filtering won't work properly
    
    # list.files(
    #   "IRA_BIL/raw/",
    #   pattern = "AccountBreakdownByAward",
    #   full.names = TRUE) %>%
    #   map(
    #     ~ read_csv(.x) %>%
    #       filter(
    #         str_detect(
    #           program_activity_name,
    #           "INFLATION REDUCTION ACT|
    #            BIPARTISAN INFRASTRUCTURE LAW|
    #           INFRASTRUCTURE INVESTMENT AND JOB ACT|
    #           INFRASTRUCTURE INVESTMENT AND JOBS ACT") |
    #           disaster_emergency_fund_name == "Infrastructure Investment and Jobs Act" |
    #           str_detect(
    #             prime_award_base_transaction_description,
    #             "INFLATION REDUCTION ACT|
    #             BIPARTISAN INFRASTRUCTURE LAW|
    #             INFRASTRUCTURE INVESTMENT AND JOB ACT|
    #             INFRASTRUCTURE INVESTMENT AND JOBS ACT")) %>%
    #       rbind(base_fileC) %>%
    #       assign(
    #         "base_fileC",
    #         .,
    #         envir = globalenv()))
    # 
    # # File B
    # 
    # list.files(
    #   "IRA_BIL/raw/",
    #   pattern = "AccountBreakdownByPA-OC",
    #   full.names = TRUE) %>%
    #   map(
    #     ~ read_csv(.x) %>%
    #       filter(
    #         str_detect(
    #           program_activity_name,
    #           "INFLATION REDUCTION ACT|
    #            BIPARTISAN INFRASTRUCTURE LAW|
    #           INFRASTRUCTURE INVESTMENT AND JOB ACT|
    #           INFRASTRUCTURE INVESTMENT AND JOBS ACT") |
    #           disaster_emergency_fund_name == "Infrastructure Investment and Jobs Act") %>%
    #       rbind(base_fileB) %>%
    #       assign(
    #         "base_fileB",
    #         .,
    #         envir = globalenv()))
    # 
    # list.files(
    #   "IRA_BIL/raw/",
    #   full.names = TRUE) %>% 
    #   unlink()
  }
}

# Test function
ira_bil_download("federal_account", "78", "2023", "3")

input_account_level <- "federal_account"
input_agency_code <- agency_code_list
# agency_info$agencies$cfo_agencies$toptier_agency_id %>%
# as.character()
input_year <- "2023"
input_num_quarters <- "3"

inputs <- 
  list(
    account_level = 
      rep(
        "federal_account",
        times = length(input_agency_code)*length(input_year)),
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
        times = length(input_agency_code)*length(input_year)))

log <- 
  pmap(
    .l = inputs,
    .f = 
      safely(ira_bil_download))

# Check errors:
# (USAspending doesn't prepare the file properly; can be solved by repeating the downloading process)

for (x in 1:length(log)) {
  
  if (!is.null(log[[x]]$error)){
    paste(
      as.character(x), 
      ":",
      as.character(log[[x]]$error)) %>% 
      print()
  }
}


check <- 
  list.files(
    "IRA_BIL/raw_2021/",
    pattern = "AccountBreakdownByPA-OC",
    full.names = TRUE) %>% 
  map(
    ~ read_csv(.x) %>% 
      pull("owning_agency_name") %>% 
      unique())


# Filter to IRA and BIL (File C) ---------------------------------------------------

list.files(
  "IRA_BIL/raw_2022/",
  pattern = "AccountBreakdownByAward",
  full.names = TRUE) %>%
  map(
    ~ read_csv(
      .x,
      col_types = cols(.default = "c")) %>%
      filter(
        str_detect(
          program_activity_name,
          "INFLATION REDUCTION ACT|
           BIPARTISAN INFRASTRUCTURE LAW|
          INFRASTRUCTURE INVESTMENT AND JOB ACT|
          INFRASTRUCTURE INVESTMENT AND JOBS ACT") |
          disaster_emergency_fund_name == "Infrastructure Investment and Jobs Act" |
          str_detect(
            prime_award_base_transaction_description,
            "INFLATION REDUCTION ACT|
            BIPARTISAN INFRASTRUCTURE LAW|
            INFRASTRUCTURE INVESTMENT AND JOB ACT|
            INFRASTRUCTURE INVESTMENT AND JOBS ACT") |
          str_detect(
            cfda_title,
            "INFLATION REDUCTION ACT|
            BIPARTISAN INFRASTRUCTURE LAW|
            INFRASTRUCTURE INVESTMENT AND JOB ACT|
            INFRASTRUCTURE INVESTMENT AND JOBS ACT")) %>%
      rbind(base_fileC) %>%
      assign(
        "base_fileC",
        .,
        envir = globalenv()))

write_csv(base_fileC, "IRA_BIL/ira_bil_fileC_2022.csv")

# base_fileC <- base_fileC[0,]

# file109_2023 <- list.files(
#   "IRA_BIL/raw_2023/",
#   pattern = "AccountBreakdownByAward",
#   full.names = TRUE)[[110]] %>% 
#   read_csv(
#     col_types = cols(.default = "c"))

# Append

fileC_2022 <- 
  base_fileC %>% 
  mutate(
    "prime_award_summary_recipient_cd_original" = NA_character_,           
    "prime_award_summary_recipient_cd_current" = NA_character_,            
    "prime_award_summary_place_of_performance_cd_original" = NA_character_,
    "prime_award_summary_place_of_performance_cd_current" = NA_character_)

fileC_2023 <- 
  read_csv("IRA_BIL/ira_bil_fileC_2023_update.csv") %>% 
  mutate(
    "recipient_congressional_district" = NA_character_,
    "primary_place_of_performance_congressional_district" = NA_character_)

output_fileC <- 
  rbind(fileC_2022, fileC_2023)

write_csv(output_fileC, "IRA_BIL/ira_bil_fileC_total.csv")


# Filter to IRA and BIL (File B) ------------------------------------------

list.files(
  "IRA_BIL/raw_2023_update/",
  pattern = "AccountBreakdownByPA-OC",
  full.names = TRUE) %>%
  map(
    ~ read_csv(
      .x,
      col_types = cols(.default = "c")) %>%
      filter(
        str_detect(
          program_activity_name,
          "INFLATION REDUCTION ACT|
           BIPARTISAN INFRASTRUCTURE LAW|
          INFRASTRUCTURE INVESTMENT AND JOB ACT|
          INFRASTRUCTURE INVESTMENT AND JOBS ACT") |
          disaster_emergency_fund_name == "Infrastructure Investment and Jobs Act") %>%
      rbind(base_fileB, .) %>%
      assign(
        "base_fileB",
        .,
        envir = globalenv()))

write_csv(base_fileB, "IRA_BIL/ira_bil_fileB_2023.csv")

# Merge

fileB_2022 <- 
  read_csv("IRA_BIL/ira_bil_fileB_2022.csv")

fileB_2023 <- 
  read_csv("IRA_BIL/ira_bil_fileB_2023.csv")

output_fileB <- 
  rbind(fileB_2022, fileB_2023)

write_csv(output_fileB, "IRA_BIL/ira_bil_fileB_total.csv")
  