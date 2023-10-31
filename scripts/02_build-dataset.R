# This file brings in the individual data sources, 
# merges them together into a master dataset, and
# from there creates the data that powers our shiny app:
  # data to generate the national maps
  # county-level raw and percentile values for indicators of interest
  # county overlap with tribal lands
  # counties' nearest neighbors

# Setup
library(tidyverse)
library(readxl)
library(janitor)


### Load in Data Sets for Merge

# ACS_demographic will be the base for merge since it has state and county names
ACS_demographic <- 
  read_csv(file.path("data", "modified-data", "ACS_demographic_clean.csv"))

COA <- 
  read_csv(file.path("data", "modified-data", "COA_clean.csv"))

FEA <- 
  read_csv(file.path("data", "modified-data", "FEA_clean.csv"))

MMG <- 
  read_csv(file.path("data", "modified-data", "MMG_clean.csv"))

tribal_land_df <- 
  read_csv(file.path("data", "modified-data", "tribal_land_clean.csv"))

sheldus_df <- 
  read_csv(file.path("data", "modified-data", "sheldus_dr_fl_wf.csv"))

CDC <- 
  read_csv(file.path("data", "modified-data", "cdc_physres_clean.csv"))

mobility_metrics <- 
  read_csv(file.path("data", "modified-data", "mobility_metrics_clean.csv"))

### Merge Data Sets

# Place dfs into a list for merge
df_list = list(ACS_demographic, FEA, MMG, tribal_land_df, sheldus_df, CDC, mobility_metrics, COA)

# Merge data sets together
full_df <- df_list %>%
  reduce(full_join, by = "geoid") %>% 
  select(geoid, county_name, state_name, everything())

### Imputation
impute_zero <- function(df, impute_vars){
  ## Imputes data by filling in zeroes for NA values 
  
  ## INPUTS:
  ## df (df): The dataframe used for analysis (with both missing and non-missing variables)
  ## impute_vars (char vector): a vector of variables whose values to impute a certain way
  
  ## RETURNS:
  ## imputed_data (df): a dataframe with only the columns whose values were imputed
  
  df %>%
    select(all_of(impute_vars)) %>%
    mutate(across(.cols = all_of(impute_vars), .fns = ~ replace_na(.x, 0)))
}


# Get list of vars to zero impute
zero_vars <- c('sheldus_losses_flooding','sheldus_losses_wildfire','sheldus_losses_drought', 'tribal_percent') 

non_zero_imp_vars <- full_df %>% 
  select(-any_of(zero_vars)) %>% 
  colnames()

shiny_imputed_df <- cbind(
  full_df %>% select(all_of(non_zero_imp_vars)),
  impute_zero(full_df, zero_vars)
)
# Add percentile ranking columns for selected variables
percentile_vars <- c('median_hh_income','per_h_owner','unemp_rate', 'per_disability','fi_rate_overall','share_hh_receive_snap',
                     'cost_per_meal', 'fi_rate_black_and_hispanic', 'pct_laccess_hhnv15','ratio_average_to_living_wage','share_production_edible_crops',
                     'share_production_edible_animals','share_commodity_direct','sheldus_losses_wildfire','sheldus_losses_drought','sheldus_losses_flooding', 
                     'ex_heat_days','ex_precip_days','tribal_percent','per_non_hisp_white','per_non_hisp_black','per_non_hisp_asian','per_hisp')

# Create percentile rank columns 
percentile_df <- shiny_imputed_df %>% 
  mutate(across(all_of(percentile_vars), ~percent_rank(.), .names = "{col}_percentile")) %>% 
  select(geoid, !!!paste0(percentile_vars, "_percentile"))

# Merge the raw and percentile values by geoid
raw_and_percentile_values_wsheldus <- merge(shiny_imputed_df, percentile_df, by = "geoid")

# Exporting scaled data for k-nearest neighbors
scaling_vars <- c('fi_rate_overall', 'fi_rate_black_and_hispanic','share_production_edible_crops','share_production_edible_animals',
                  'sheldus_losses_wildfire','sheldus_losses_drought','sheldus_losses_flooding')

knn_input_data <- raw_and_percentile_values_wsheldus %>% 
  mutate(across(all_of(scaling_vars), scale)) %>% # Normalizing
  mutate(across(all_of(scaling_vars), ~as.vector(.))) %>% #Ensuring columns are not matrix array so can export to csv
  select(geoid, all_of(scaling_vars)) # Selecting only knn vars

knn_input_data %>% 
  write_csv(file = file.path("data", "modified-data", "knn_input_data.csv"))

#Excluding observations that are zero all across for knn 
sheldus_knn_input_data <- raw_and_percentile_values_wsheldus %>% 
  filter(sheldus_losses_flooding != 0 | sheldus_losses_wildfire != 0 | sheldus_losses_drought != 0) %>% 
  mutate(across(all_of(scaling_vars), scale)) %>% # Normalizing
  mutate(across(all_of(scaling_vars), ~as.vector(.))) %>% #Ensuring columns are not matrix array so can export to csv
  select(geoid, all_of(scaling_vars)) # Selecting only knn vars

sheldus_knn_input_data %>% 
  write_csv(file = file.path("data", "modified-data", "sheldus_knn_input_data.csv"))

# Exporting raw and percentile data for Shiny App Descriptives
raw_and_percentile_values <- raw_and_percentile_values_wsheldus %>% 
  select(-sheldus_losses_wildfire,-sheldus_losses_drought,-sheldus_losses_flooding)

raw_and_percentile_values %>% 
  write_csv(file = file.path("data", "modified-data", "raw_and_percentile.csv"))

### National Maps

raw_and_percentile_values_wsheldus_filtered <- raw_and_percentile_values_wsheldus %>% 
  # Want only counties with nonzero damages to get more meaningful results in the nearest neighbors analysis
  filter(sheldus_losses_flooding != 0 | sheldus_losses_wildfire != 0 | sheldus_losses_drought != 0) %>% 
  mutate(share_production_edible_overall = share_production_edible_crops + share_production_edible_animals) %>% # Creating joint share of edible production
  mutate(sheldus_average = (sheldus_losses_drought + sheldus_losses_flooding + sheldus_losses_wildfire) / 3) 

# Normalize and create map indices 
map_vars <- c('fi_rate_overall', 'fi_rate_black_and_hispanic','share_production_edible_overall',
              'sheldus_losses_wildfire','sheldus_losses_drought','sheldus_losses_flooding', 'sheldus_average')

map_input_data <- raw_and_percentile_values_wsheldus_filtered %>% 
  mutate(across(all_of(map_vars), scale)) %>% # Normalizing
  mutate(across(all_of(map_vars), ~as.vector(.))) %>% #Ensuring columns are not matrix array so can export to csv
  select(geoid, all_of(map_vars)) # Selecting only map vars

indices <- c('fi_rate_black_and_hispanic', 'share_production_edible_overall', 'sheldus_average')

map_input_data <- map_input_data %>% 
  mutate(across(all_of(indices), ~as.vector(.))) %>% 
  mutate(across(all_of(indices), ~percent_rank(.), .names = "{col}_p"))

# Create categorical quintile variables
quint_cat <- function(df, vars){
  final <- df
  
  for (var in vars) {
    quint <- quantile(df[[var]], probs = seq(0,1, by = 0.2), na.rm = TRUE)
    
    df[[paste0(var, "_quintile")]]  <- cut(df[[var]], breaks = quint, labels = c(1, 2, 3, 4 ,5), include.lowest = TRUE)
    
    final <- cbind(final, df[paste0(var, "_quintile")])
  }
  return(final)
}

map_quintile_data <- quint_cat(map_input_data, c('fi_rate_overall', 'fi_rate_black_and_hispanic','share_production_edible_overall', 'sheldus_average'))


# Generating data for top quintile
map_quintile_data$sa_q5 <- ifelse(map_quintile_data$sheldus_average_quintile == "5", 1, 0)
map_quintile_data$fi_q5 <- ifelse(map_quintile_data$fi_rate_overall_quintile == "5", 1, 0)
map_quintile_data$fi_bh_q5 <- ifelse(map_quintile_data$fi_rate_black_and_hispanic_quintile == "5", 1, 0)
map_quintile_data$edible_q5 <- ifelse(map_quintile_data$share_production_edible_overall_quintile == "5", 1, 0)


map_quintile_data %>% 
  write_csv(file = file.path("data", "modified-data", "map_input_data.csv"))

