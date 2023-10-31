# Food Systems Resilience and Equity

This repository contains the code necessary to generate the county-level Food Systems Resilience and Equity dataset that powers [this Rshiny application](https://urban-institute.shinyapps.io/food-systems-clustering/).

For more information on the raw data sources and methodology and to download the final harmonized datasets, visit Urban's [data catalog](https://datacatalog.urban.org/dataset/food-systems-resilience-and-equity).

## Harmonized Datasets
- `raw_and_percentile.csv`: Raw and percentile county-level data with a range of indicators on food systems resilience and equity
- `map_input_data.csv`: Data for generating the national county-level maps shown in the RShiny app
- `nearest_neighbors.csv`: Data on each county's 5 "nearest neighbors" for food security and access, agricultural production, and climate hazards as shown in the RShiny app
- `tribal_land_clean.csv`: Data on the percentage of each county's land area that overlaps with tribal lands as shown in the RShiny app

## Scripts
- `01_clean-data.R`: this reads in and cleans each of the individual datasets used in the RShiny app
- `02_build-dataset.R`: this harmonizes the individual data and performs necessary wrangling to build the data powering the RShiny app
- `03_generate-knn.R`: this runs a 5-nearest neighbors analysis for each county separately for each of three different domains: food access, food production, and climate hazards
- `04_shiny-app-helper.R`: this is a helper function for the Shiny app that does some final data manipulation
- `app.R`: this is the script that builds the Shiny application
