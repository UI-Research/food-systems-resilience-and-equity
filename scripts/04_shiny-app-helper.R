library(tidyverse)
library(sf)
library(DT)
library(urbnmapr)
library(leaflet)
library(htmltools)

# list of states
state_list <- state.name

county_list <- urbnmapr::counties %>% 
  distinct(county_name, state_name, county_fips)

# shapefile of counties for map
county_sf <- tigris::counties(resolution = "20m", cb = TRUE)  
  # drop AK and HI for mapping
  # filter(!STATEFP %in% c("02", "15"))

# read in raw and percentile data
raw_perc <- read_csv(file.path("data", "modified-data", "raw_and_percentile.csv")) %>% 
  # join county data
  left_join(county_list, by = c("geoid" = "county_fips")) %>% 
  # scale to a decimal percent
  mutate(tribal_percent = tribal_percent / 100, 
         share_ex_heat_days = share_ex_heat_days/100, 
         share_ex_precip_days = share_ex_precip_days/100)


# read in KNN data
knn <- read_csv(file.path("data", "modified-data", "nearest_neighbors.csv")) %>% 
  left_join(county_list, by = c("geoid" = "county_fips")) %>% 
  left_join(county_list, by = c("geoid_neighbor" = "county_fips"), suffix = c("_og","_neighbor"))

# map data
map_data <- read_csv(file.path( "data", "modified-data", "map_input_data.csv")) %>%
  # join county data
  tidylog::right_join(county_list, by = c("geoid" = "county_fips")) %>% 
  # join tribal land data
  left_join(read_csv(file.path("data", "modified-data", "tribal_land_clean.csv"))) %>% 
  # high food insecurity high sheldus
  # high fi black high sheldus
  # high ag production high sheldus
  mutate(fi_sheldus = case_when(sa_q5 == 1 & fi_q5 == 1 ~ "Both",
                                sa_q5 == 1 & fi_q5 == 0 ~ "High Climate Risk",
                                sa_q5 == 0 & fi_q5 == 1 ~ "High Food Insecurity",
                                TRUE ~ "Lower Risk") %>% factor(levels = c("High Food Insecurity", "High Climate Risk", "Both", "Lower Risk")), 
         fi_bh_sheldus = case_when(sa_q5 == 1 & fi_bh_q5 == 1 ~ "Both",
                                   sa_q5 == 1 & fi_bh_q5 == 0 ~ "High Climate Risk",
                                   sa_q5 == 0 & fi_bh_q5 == 1 ~ "High Black and Hispanic Food Insecurity",
                                   TRUE ~ "Lower Risk") %>% factor(levels = c("High Black and Hispanic Food Insecurity", "High Climate Risk", "Both", "Lower Risk")),
         edible_sheldus = case_when(sa_q5 == 1 & edible_q5 == 1 ~ "Both",
                                    sa_q5 == 1 & edible_q5 == 0 ~ "High Climate Risk",
                                    sa_q5 == 0 & edible_q5 == 1 ~ "High Edible Agricultural Production",
                                    TRUE ~ "Lower Risk") %>% factor(levels = c("High Edible Agricultural Production", "High Climate Risk", "Both", "Lower Risk")),
         tribal_sheldus = case_when(sa_q5 == 1 & !is.na(tribal_percent) ~ "Tribal Lands and High Climate Risk",
                                    tribal_percent != 0 ~ "Tribal Lands", 
                                    TRUE ~ "Lower Risk") %>% factor(levels = c("Tribal Lands and High Climate Risk", "Tribal Lands", "Lower Risk"))) 

# sheldus variables
sheldus_vars <- map_data %>% 
  select(sheldus_average, sheldus_average_p, geoid)

# read in raw and percentile data
raw_perc <- read_csv("data/modified-data/raw_and_percentile.csv") %>% 
  # join county data
  left_join(county_list, by = c("geoid" = "county_fips")) %>% 
  # scale to a decimal percent
  mutate(tribal_percent = tribal_percent/100, 
         pct_laccess_hhnv15 = pct_laccess_hhnv15/100, 
         share_ex_heat_days = share_ex_heat_days/100, 
         share_ex_precip_days = share_ex_precip_days/100,
         ratio_black_nh_house_value_households = ratio_black_nh_house_value_households,       
         ratio_hispanic_house_value_households = ratio_hispanic_house_value_households,     
         ratio_white_nh_house_value_households = ratio_white_nh_house_value_households,        
         ratio_other_nh_house_value_households = ratio_other_nh_house_value_households) %>% 
  left_join(sheldus_vars) 

# read in KNN data
knn <- read_csv("data/modified-data/nearest_neighbors.csv") %>% 
  left_join(county_list, by = c("geoid" = "county_fips")) %>% 
  left_join(county_list, by = c("geoid_neighbor" = "county_fips"), suffix = c("_og","_neighbor"))

