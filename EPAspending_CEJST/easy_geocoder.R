# EASY GEOCODER
# -- Xinyu ZHENG

# FUNCTION 1: single address match ----------------------------------------

## setup -------------------------------------------------------------------

requirement <- 
  c("httr", "jsonlite", "tidyverse")

for (package_name in requirement) {
  if (require(package_name, character.only = TRUE)) {
    library(package_name,character.only = TRUE)
  } else {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}

rm(requirement)


## sample dataset ----------------------------------------------------------

df <- read_csv("geocoder_eg.csv")[1:50,]

## function: preprocess and match ------------------------------------------

geocoder_api <- function(df, geographics){
  
  columns <- c("street", "city", "state", "zipcode")
  
  # Check if the input table contains necessary columns
  
  check_column <- 
    columns %in% names(df)
  
  if (mean(check_column) != 1) {
    
    print(
      paste0("ERROR: The input table does not have column ",
             columns[which(check_column == FALSE)]))
    
    break
  }
  
  # Check if all fields in the street column are not NAs
  
  check_na <- is.na(df$street)
  
  if (mean(check_na) != 0) {
    
    print("WARNING: The street column includes empty cells.")
    
    na_answer <- 
      readline(
        prompt = 
          paste0("Do you want to proceed by ignoring those cells?", 
                 "Please type 'Yes' or 'No': "))
    
    ## If choose to ignore all the NAs
    
    if (na_answer == "Yes") {
      df <-
        filter(df, !is.na(street))
    } 
    
    ## If choose to stop running the function
    
    else {
      print(
        paste0("Please check row: ",
               which(check_na == TRUE)))
      break
    }
  }
  
  # Check if the zipcode column is correctly formatted
  
  ## Check if zipcode column is string
  
  zipcode_string <- 
    class(df$zipcode) == "character"
  
  if (zipcode_string == FALSE) {
    
    tryCatch(
      {df <- 
        mutate(df, zipcode = as.character(zipcode))},
      error = function(e){
        message("Please follow the error message below to detect problems in zip code column")
        print(e)
      })
  }
  
  ## Check if zipcodes are not corrupted 
  
  zipcode_digit <- 
    df$zipcode %>% 
    map(~ nchar(.x)) %>% 
    reduce(c)
  
  zipcode_nonstringn <- 
    str_detect(df$zipcode, "\\D")
  
  if (min(zipcode_digit) < 5 | mean(zipcode_nonstringn) != 0) {
    
    zipcode_corrupt_answer <- 
      readline(prompt = 
                 paste0("WARNING: Some ZIP codes are corrupted.", 
                        "\nForced matching might yield inaccurate results.", 
                        "\nDo you want to proceed anyways or delete these rows?", 
                        "\nPlease type 'Keep', 'Delete' or 'Cancel': "))
    
    ### If choose to keep going without changes
    
    if (zipcode_corrupt_answer == "Keep") {
      print("Continue matching process with corrupted zipcodes")
    } 
    
    ### If choose to delete all corrupted rows
    
    else if (zipcode_corrupt_answer == "Delete") {
      df <- 
        cbind(df, zipcode_digit, zipcode_nonstringn) %>%
        filter(zipcode_digit >= 5,
               zipcode_nonstringn == 0) %>%
        select(-zipcode_digit, -zipcode_nonstringn)
      
      print("Corrupted rows deleted ")
    } 
    
    ### If choose to stop running the function
    
    else {
      print(
        paste("Matching process canceled. Check rows: ",
              which(zipcode_digit < 5 | zipcode_nonstringn == TRUE)))
    }
  }
  
  # All words in street, city, and state columns should be separated by "+"
  
  input_df <- 
    df %>%
    mutate(
      across(
        c(street, city, state),
        ~ str_replace_all(.x, " ", "+")))
  
  # Prepare a empty table for appended results
  
  matched_eg <-
    GET(
      url = 
        paste0("https://geocoding.geo.census.gov/geocoder/geographies/address?",
               "street=9201+OAKDALE+AVE+STE+101&city=CHATSWORTH&state=CALIFORNIA",
               "&zip=91311&benchmark=Public_AR_Census2020",
               "&vintage=Census2020_Census2020&format=json")) %>% 
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON()
  
  matched_df <- 
    matched_eg[["result"]][["addressMatches"]][["geographies"]][[geographics]][[1]]
  
  # API function
  
  for (i in 1:nrow(input_df)) {
    
    url <- 
      paste0(
        "https://geocoding.geo.census.gov/geocoder/geographies/address?",
        "street=",
        input_df$street[i],
        "&city=",
        input_df$city[i],
        "&state=",
        input_df$state[i],
        "&zip=",
        input_df$zipcode[i],
        "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json")
    
    response <- 
      tryCatch({
        GET(url = url) %>%
          content(as = "text", encoding = "UTF-8") %>%  # Specify the encoding here
          fromJSON()
      },
      error = function(e) {
        warning(
          paste("Request failed for row", i, ":", e))
        NULL
      })
    
    # If there's an error:
    
    if (is.null(response)) {
      
      matched_info <- NA_character_
      
    }
    else {
      
      match <-
        response[["result"]][["addressMatches"]] %>%
        length()
      
      # If there's no matched results
      
      if (match == 0){
        
        matched_info <- NA_character_
      }
      
      # Find matched gepgraphic info
      
      else {
        
        matched_info <-
          response[["result"]][["addressMatches"]][["geographies"]][[geographics]][[1]]
        
      }
    }
    
    # Append the matched results
    
    matched_df <- rbind(matched_df, matched_info)
    
    # Optionally add a delay to avoid hitting rate limits
    
    # print(
    #  paste("Finished row:", i))
    
    Sys.sleep(0.1)
  }
  
  
  final_matched_df <- 
    cbind(df, matched_df[2:nrow(matched_df),])
  
  assign(
    "final_matched_df",
    final_matched_df,
    envir = globalenv())
  
}

# Example of using the function:

# State Legislative Districts - Upper
# States
# Combined Statistical Areas
# County Subdivisions
# State Legislative Districts - Lower
# Urban Areas
# Incorporated Places
# Counties
# Census Tracts
# Census Blocks

geocoder_api(df = df, geographics = "Counties")




# FUNCTION 2: batch match  (Under development)-------------------------------------------------

## Break the file into several files with 10000 rows each ------------------

address_total <- 
  read_csv("address_geoid/EPAspending_financialassistant_prime_2011_2023.csv") %>% 
  filter(
    !is.na(recipient_address_line_1),
    !is.na(recipient_city_name),
    !is.na(recipient_state_name)) %>% 
  mutate(zipcode = "")

i <- 1

while ((i*10000) < nrow(address_total)){
  
  address_total[((i-1)*10000 + 1):(i*10000),] %>% 
    write.table(
      file = paste0("address_geoid/input/fa_address_", as.character(i), ".csv"),
      sep = ",",
      col.names = FALSE,
      row.names = FALSE)
  
  i = i + 1
}

address_total[((i-1)*10000 + 1):nrow(address_total),] %>% 
  write.table(
    paste0("address_geoid/input/fa_address_", as.character(i), ".csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE)


## loop the request for the address matching -------------------------------

geocode <- function(input_file){
  
  geocoder_body <- 
    list(
      addressFile = 
        upload_file(input_file),
      benchmark = "Public_AR_Census2020",
      vintage = "Census2020_Census2020")
  
  response <- 
    POST(
      url = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch",
      body = geocoder_body,
      encode = "multipart")
  
  response_df <- 
    content(response, as = "text") %>% 
    read_csv(col_names = FALSE)
  
  names(response_df) <- 
    c("contract_transaction_unique_key",
      "origin_address",
      "match_status",
      "match_accuracy",
      "matched_address",
      "longitude_latitute",
      "tigerline_id",
      "tigerline_id_side",
      "state_code",
      "county_code",
      "census_tract_code",
      "block_code")
  
  write_csv(
    response_df, 
    paste0("address_geoid/output/fa_geoid_", 
           str_extract(input_file, "\\d+"),
           "_matched.csv"))
}

input_file_list <- 
  list.files(
    "address_geoid/input",
    pattern = "fa",
    full.names = TRUE)


map(.x = input_file_list, .f = geocode)


## trouble shooting --------------------------------------------------------

test <- read_csv("address_geoid/input/fa_address_4.csv", col_names = FALSE)

test %>%
  mutate(
    X2 = str_replace(X2, "\\\\", ""),
    X3 = str_replace(X3, "\\.", ""),
    # X3 = str_replace(X3, "\\'", " "),
    X5 = "") %>%
  write.table("address_geoid/input/fa_address_4_update.csv",
              sep = ",",
              col.names = FALSE,
              row.names = FALSE)

## reproduce spending data with geoid --------------------------------------

fa <- 
  list.files(
    "address_geoid/output/",
    pattern = "fa",
    full.names = TRUE) %>% 
  map(
    ~ read_csv(.x)) %>% 
  reduce(rbind)

award <- 
  list.files(
    "address_geoid/output/",
    pattern = "award",
    full.names = TRUE) %>% 
  map(
    ~ read_csv(.x)) %>% 
  reduce(rbind)

