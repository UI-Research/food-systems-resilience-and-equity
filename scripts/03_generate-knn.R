# This script generates each county's 5 nearest neighbors
# across three different domains:
  # food acccess
  # food production
  # climate hazards

library(tidyverse)
library(dbscan)

k <- 5
sheldus_df <- read_csv(file.path("data", "modified-data", "sheldus_knn_input_data.csv")) %>% 
  select(geoid, sheldus_losses_flooding, sheldus_losses_drought, sheldus_losses_wildfire) %>% 
  tibble()

knn_input_data <- read_csv(file.path("data", "modified-data", "knn_input_data.csv"))

food_produc_df <-  knn_input_data %>% 
  filter(!is.na(share_production_edible_crops)) %>% #Deleting missing counties because KNN requires full data
  select(geoid, share_production_edible_crops, share_production_edible_animals) %>% # Removed share_commodity_direct because of high missingness 
  tibble()

food_access_df <- knn_input_data %>% 
  filter(!is.na(fi_rate_black_and_hispanic)) %>% #Again deleting missing counties
  select(geoid, fi_rate_overall, fi_rate_black_and_hispanic) %>%  
  tibble()

# Generate KNN function, which returns K nearest neighbors and their distances
nearest_neighbors <- function(df,k) {
  df = sheldus_df
  # Create crosswalk from row number to geoid
  geoids <- df %>% 
    select(geoid) %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname))
  
  # Run KNN
  knn <- kNN(df %>% select(-geoid), k, sort=TRUE) 
  knn_dists <- as_tibble(knn$dist) %>% rename_with(~ paste0('distance', .x)) 
  knn_counties <- as_tibble(knn$id) %>% rename_with(~ paste0('neighbor', .x))
  
  # Generate final dataset with each county, its K nearest neighbors (across the variables run in kNN above), and their Euclidean distances from the reference county
  knn_final <- cbind(geoids$geoid, knn_dists, knn_counties) %>%
    pivot_longer(cols = starts_with('neighbor')) %>%
    left_join(geoids, by = c('value'='rowname')) %>%
    pivot_wider(id_cols=`geoids$geoid`:distance5, names_from = name, values_from = geoid) %>%
    rename(geoid=`geoids$geoid`)
  
  knn_final_long <- knn_final %>% 
    pivot_longer(
      cols = starts_with("neighbor"),
      names_to = "neighbor_number",
      values_to = "geoid_neighbor",
      names_prefix = "neighbor",
      values_drop_na = TRUE) %>% 
    select(geoid, neighbor_number, geoid_neighbor)
  
  return(knn_final_long)
}

### Generate 5 nearest neighbors for climate risk, food production, and food access

knn_sheldus_long <- nearest_neighbors(sheldus_df, k) %>% 
  mutate(risk_type = "Climate") 

knn_food_access_long<- nearest_neighbors(food_access_df, k) %>% 
  mutate(risk_type = "Food Access")

knn_food_produc_long <- nearest_neighbors(food_produc_df, k) %>% 
  mutate(risk_type = "Food Production")

# Append all clusters
final_knn <- bind_rows(knn_sheldus_long, knn_food_produc_long, knn_food_access_long)

final_knn %>% 
  write_csv(file = file.path("data", "modified-data", "nearest_neighbors.csv"))
