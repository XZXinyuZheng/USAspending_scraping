# USAspending_scraping

## Overview

This repo provides codes to automate scraping data from [USAspending.gov](https://www.usaspending.gov/)

In responding to the Justice 40 initiative, tracking federal spending to see if it has been flowing to disadvantaged communities is meaningful. USAspending.gov is the official website to record all transactions of government contracts or financial assistance. 

Downloading all federal spending data from USAspending.gov requires over 1.5 T of computer storage space, which is impossible for most users. And the click-and-point customized downloading on USAspending.gov’s website for specific agencies or programs is tedious. I wrote scraping scripts to focus only on the spending of agencies or programs related to Environmental Justice and increase the replicability of the downloading processes.

This project only focuses on spending related to the Inflation Reduction Act (IRA) and Bipartisan Infrastructure Law (BIL) programs because those programs target clean energy and the climate change sector. The data of certain agencies (Environmental Protection Agency, EPA) is also included in this project for its high relevance to environmental justice.

## Instruction

* __usaspending-EPA.R__ includes codes to scrape agency account and award data. Here, I take the example of EPA. Tables will be saved in the folder EPA and its subfolders.
* __usaspending-IRA_BIL.R__ includes codes to scrape data related to specific programs. I scraped data related to the Inflation Reduction Act and Bipartisan Infrastructure Law here. Tables will be saved in the folder IRA_BIL and its subfolders.
* __EPAspending_CEJST(Foder)__ includes a script __easy_geocoder.R__ to match street addresses with different levels of geographics automatically. Please see more detail in the README of this folder.

## More Information

This project was developed for the [Environmental Impact Data Collaborative (EIDC)](https://mdi.georgetown.edu/eidc/) at the Massive Data Institute of Georgetown University. Access to the [EIDC platform](https://redivis.com/EIDC) holding the data products requires an application. If interested, please contact EIDC through eidc@georgetown.edu.