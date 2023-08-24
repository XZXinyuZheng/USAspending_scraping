library(tidyverse)
library(httr)
library(jsonlite)


# Break the file into several files with 10000 rows each ------------------


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


# loop the request for the address matching -------------------------------

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


# trouble shooting --------------------------------------------------------

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


# reproduce spending data with geoid --------------------------------------

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





test <- 
  read_csv("address_geoid/input/award_address_14.csv", col_names = FALSE) %>% 
  mutate(
    across(
      c("X2", "X3", "X4"), 
      ~ str_replace_all(.x, " ", "+")))

geocoder_api <- function(street, city, state){
  
  url <- 
    paste0(
      "https://geocoding.geo.census.gov/geocoder/geographies/address?",
      "street=",
      street,
      "&city=",
      city,
      "&state=",
      state,
      "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json")
  
  response <- 
    GET(url = url) %>% 
    content(as = "text") %>% 
    fromJSON()
  
  return(response)
  
  # geoid <- 
  #   response[["result"]][["addressMatches"]][["geographies"]][["Census Tracts"]][[1]][["GEOID"]]
  # 
  # if (is.null(geoid)){
  #   geoid <- NA_character_
  # }
  # 
  # return(geoid)
}

input_list <- 
  list(
    street = test[c(1, 28, 427),]$X2,
    city = test[c(1, 28, 427),]$X3,
    state = test[c(1, 28, 427),]$X4)

geoid_vector <- 
  pmap(
    .l = input_list,
    .f = geocoder_api) %>% 
  reduce(c)

response <- 
  geocoder_api("250+MCDERMOT+AVE+SUITE+401", "WINNIPEG", "MB")

# general function --------------------------------------------------------

geocoder_api <- function(df){
  
  if (!("street" %in% names(df))){
    
    print("This data frame does not contain essential columns to run this function. Please check if this data frame misses a street address column called 'street'.")
    
    break
  }
  else if (!(c("city", "state", "zipcode") %in% names(df))) {
    
  }
  else {
    
    input_df <- 
      df %>%
      mutate(
        across(
          c(street, city, state),
          ~ str_replace_all(.x, " ", "+")))
    
    url <- 
      paste0(
        "https://geocoding.geo.census.gov/geocoder/geographies/address?",
        "street=",
        street,
        "&city=",
        city,
        "&state=",
        state,
        "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json")
    
    response <- 
      GET(url = url) %>% 
      content(as = "text") %>% 
      fromJSON()
    
    geoid <- 
      response[["result"]][["addressMatches"]][["geographies"]][["Census Tracts"]][[1]][["GEOID"]]
    
    if (is.null(geoid)){
      geoid <- NA_character_
    }
    
    return(geoid)
  }
  
}  
