# USAspending_scraping - Easy Geocoder

## Overview

For privacy considerations, communties might not be willing to give out information associated with street addresses. One solution is to aggregate data to census tract or county level, making it necessary to build up a tool to match street addresses with different levels of geographics.

This notebook includes two functions to match the street address with geographics through the Census Bureau Geocoder.

* __Function 1: single_match:__ It takes a single piece of address information as the input and returns matched address with detailed geographics.

* __Function 2: batch_match:__ It automatically upload a input file containing multiple address information into Geocoder and return a table with matched results.

## Prerequisite
Before running single_match and batch_match, please make sure to format your input table in the following format:

* Include one column of unique row identifier (no naming conversion)
* Include one column of street address named as "street"
* Include one column of city named as "city"
* Include one column of state abbreviatioin or full name named as "sate"
* Include one column of 5-digit ZIP code named as "zipcode"

Additional to the requriement above, the input file for running function batch_match has to be:

* In .csv extension
* Without headers
* Contains less than or equal to 10,000 rows.