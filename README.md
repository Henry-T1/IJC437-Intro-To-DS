# IJC437 Introduction To Data Science Coursework

This repository contains the code, cleaned data, and graphical outputs for the IJC437 Coursework

## What is the objective of the report?

This report investigates the presence of synoptic regimes which influence local concentrations of NO2 and 
PM2.5 in South Yorkshire.

Air quality and meteorological data are downloaded from two open APIs, cleaned, combined, and analysed through 
application of PCA, K-means clustering and Random Forest classification.

## How this repository is structured

- `code/` - contains two R scripts:
   - `IJC437_Download_And_Structure.R`
       This script contains the code to download and structure the API data. The API downloads are substantial
       and produce large raw data files. This code uses two functions to download the data which were produced
       with the assistance of ChatGPT. 
   - `IJC437_Analyse.R`
      This script contains the code to perform the analysis and produce the figures used in the report.

- `data/` 
This file contains the three cleaned data sets used in IJC437_Analyse, the clean air quality, clean weather,
and cleaned combined data.

- `figures/`
This file contains all of the figures used in the report, labelled with their corresponding figure number.

## How do you run the code?

1) Download the project
2) Open the project folder in RStudio
3) Confirm working directory includes `code/`,`data/` and `figures/` files
  a) If necessary, set working directory:
```r
 setwd("~/Documents/Introduction to Data Science")
```
4) Run the download and structure script: >**Note** Downloads are large and can take sveral hours to complete
```r
 source("code/IJC437_Download_And_Structure.R")
```
5) Run the analysis script:
```r
 source("code/IJC437_Analyse.R")
```
