# This file imports and checks the following data sources: 
  # Map the Meal Gap (MTMG)
  # Food Environment Atlas (FEA)
  # American Community Survey (ACS)
  # 2017 Census of Agriculture (COA)
  # Tribal Land Area
  # CDC National Environmental Public Health Tracking Network
  # Mobility Metrics

####----Set-Up----####

# Packages
library(tidyverse)
library(readxl)
library(janitor)
library(tidycensus)
library(dotenv)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# To do: 
# 1. Create a new text file and save it in the same folder as this script, called ".env"
# 2. In that file, enter CENSUS_API_KEY=[your key]
# 3. Run the next two lines of code
load_dot_env()
census_api_key(Sys.getenv("CENSUS_API_KEY"))

####----Map the Meal Gap (MTMG)----####

# Import MTMG county level data
MMG_df_raw <- read_excel(
  path = file.path("data", "raw-data", "MMG2022_2020-2019Data_ToShare.xlsx"),
  sheet = "County",
  col_names = TRUE
)

# Import MTMG - Black and Hispanic FI rate 
MMG_df_B_H_fi_raw <- read_excel(
  path = file.path("data", "raw-data", "MMG_FI_B_H_raw.xlsx"),
  sheet = "county",
  col_names = TRUE
)

# Clean names and assign to new df
MMG_df_raw <- MMG_df_raw %>% 
  janitor::clean_names()

# Merge new Black and Hispanic FI Rate
MMG_df_1 <- left_join(x = MMG_df_raw, y = MMG_df_B_H_fi_raw, 
                    by = "fips")

# Select necessary variables and rename for brevity
MMG_df_2 <- MMG_df_1 %>% 
  select(fips, state.x, county_state, year, cost_per_meal_1_year, overall_food_insecurity_rate_1_year, 
         food_insecurity_rate_among_black_persons_all_ethnicities, 
         food_insecurity_rate_among_hispanic_persons_any_race, 
         food_insecurity_rate_among_white_non_hispanic_persons, black_hispanic_fi) %>% 
  rename(
    cost_per_meal = cost_per_meal_1_year,
    fi_rate_overall = overall_food_insecurity_rate_1_year,
    fi_rate_black_any_eth = 
      food_insecurity_rate_among_black_persons_all_ethnicities,
    fi_rate_hisp_any_race = 
      food_insecurity_rate_among_hispanic_persons_any_race,
    fi_rate_white_non_hisp = food_insecurity_rate_among_white_non_hispanic_persons,
    state = state.x,
    fi_rate_black_and_hispanic = black_hispanic_fi
  ) %>% 
  mutate(fi_rate_black_and_hispanic = as.numeric(fi_rate_black_and_hispanic)) 

# Fix geoid (fips) and filter to 2020
MMG_df_3 <- MMG_df_2 %>% 
  mutate(fips = as.character(fips),
         fips = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  rename(geoid = fips) %>% 
  filter(year == 2020) %>% 
  select(-year)

modified_data_path = file.path("data", "modified-data")
if (!file.exists(modified_data_path)) {dir.create(modified_data_path)}

# Save clean data as csv
MMG_df_clean <- MMG_df_3 %>%
  select(-state, -county_state) %>%
  write_csv(file = file.path("data", "modified-data", "MMG_clean.csv"))

####---Food Environment Atlas (FEA)----####

# Import FEA data
FEA_df_raw <- read_csv(file.path("data", "raw-data", "FEA_StateAndCountyData.csv"))

# Clean names and assign to new df
FEA_df_1 <- FEA_df_raw %>% 
  janitor::clean_names()

FEA_df_2 <- FEA_df_1 %>% 
  filter(variable_code == 'PCT_LACCESS_HHNV15') %>% 
  pivot_wider(
    id_cols = fips:county,
    id_expand = F,
    names_from = variable_code,
    values_from = value
  ) %>% 
  janitor::clean_names()

FEA_df_3 <- FEA_df_2 %>% 
  mutate(fips = as.character(fips),
         fips = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  rename(geoid = fips) %>% 
  # fixing geoid's that have changed since 2010 and/or 2015 
  mutate(geoid = case_when( 
    geoid == "46113" ~ "46102",
    geoid == "02270" ~ "02158",
    geoid == "02261" ~ "02063",
    TRUE ~ geoid
  ))

# need to duplicate 02063 and change it to 02066 since 02261 is split into both of these geoid's after 2015
FEA_df_02066 <- FEA_df_3 %>% 
  filter(geoid == "02063") %>% 
  mutate(geoid = "02066")

# adding 02066 to main df
FEA_df_4 <- FEA_df_3 %>% 
  bind_rows(FEA_df_02066)

# Save clean data as csv
FEA_df_clean <- FEA_df_4 %>% 
  select(geoid, pct_laccess_hhnv15) %>% 
  write_csv(file = file.path("data", "modified-data", "FEA_clean.csv"))


####----American Community Survey (ACS)----####

# Note: Can load ACS vars to find necessary variable codes
# acs_vars <- load_variables(2021, "acs5", cache = TRUE)

# Vars to fetch from ACS
acs_demographic_vars <- c(
  total_pop_ = "B01003_001",
  ethnicity_denom_ = "B03002_001", 
  non_hisp_total_ = "B03002_002", 
  non_hisp_white_ = "B03002_003", 
  non_hisp_black_ = "B03002_004",
  non_hisp_native_ = "B03002_005", 
  non_hisp_asian_ = "B03002_006", 
  non_hisp_pi_ = "B03002_007",
  
  hisp_total_ = "B03002_012",
  hisp_white_ = "B03002_013",
  hisp_black_ = "B03002_014",
  hisp_native_ = "B03002_015",
  hisp_asian_ = "B03002_016",
  hisp_pi_ = "B03002_017",
  
  # Unemployment
  civilian_labor_force_ = "B23025_003",
  unemployed_ = "B23025_005",
  
  # Disability Status
  disability_total_ = "B18101_001",
  disability_m_under5_ = "B18101_004",
  disability_m_5_17_ = "B18101_007",
  disability_m_18_34_ = "B18101_010",
  disability_m_35_64_ = "B18101_013",
  disability_m_65_74_ = "B18101_016",
  disability_m_75_over_ = "B18101_019",
  disability_f_under5_ = "B18101_023",
  disability_f_5_17_ = "B18101_026",
  disability_f_18_34_ = "B18101_029",
  disability_f_35_64_ = "B18101_032",
  disability_f_65_74_ = "B18101_035",
  disability_f_75_over_ = "B18101_038",

  # Home ownership by race/ethnicity
  tenure_estimate_total_ = "B25003_001", 
  tenure_owner_occ_ = "B25003_002",
  
  tenure_total_white_ = "B25003A_001", 
  tenure_owner_occ_white_ = "B25003A_002",
  
  tenure_total_non_hisp_white_ = "B25003H_001", 
  tenure_owner_occ_non_hisp_white_ = "B25003H_002",
  
  tenure_total_black_ = "B25003B_001", 
  tenure_owner_occ_black_ = "B25003B_002",
  
  tenure_total_native_ = "B25003C_001", 
  tenure_owner_occ_native_ = "B25003C_002",
  
  tenure_total_asian_ = "B25003D_001", 
  tenure_owner_occ_asian_ = "B25003D_002",
  
  tenure_total_pi_ = "B25003E_001", 
  tenure_owner_occ_pi_ = "B25003E_002",
  
  tenure_total_hisp_ = "B25003I_001", 
  tenure_owner_occ_hisp_ = "B25003I_002",
  
  # 100% FPL
  fpl_est_tot = "B17020_001",
  fpl_below = "B17020_002",
  
  fpl_est_tot_white_ = "B17020A_001",
  fpl_below_white_ = "B17020A_002",
  
  fpl_est_tot_white_non_hisp_ = "B17020H_001",
  fpl_below_white_non_hisp_ = "B17020H_002",
  
  fpl_est_tot_black_ = "B17020B_001",
  fpl_below_black_ = "B17020B_002",
  
  fpl_est_tot_native_ = "B17020C_001",
  fpl_below_native_ = "B17020C_002",
  
  fpl_est_tot_asian_ = "B17020D_001",
  fpl_below_asian_ = "B17020D_002",
  
  fpl_est_tot_pi_ = "B17020E_001",
  fpl_below_pi_ = "B17020E_002",
  
  fpl_est_tot_hisp_ = "B17020I_001",
  fpl_below_hisp_ = "B17020I_002",
  
  # Median household income
  median_hh_income_ = "B19013_001",
  median_hh_income_white_ = "B19013A_001", 
  median_hh_income_non_hisp_white_ = "B19013H_001", 
  median_hh_income_black_ = "B19013B_001", 
  median_hh_income_native_ = "B19013C_001", 
  median_hh_income_asian_ = "B19013D_001",
  median_hh_income_pi_ = "B19013E_001",
  median_hh_income_hisp_ = "B19013I_001",
  
  # Households Receiving Snap
  num_hh_snap_est_ = "B22003_001",
  num_hh_receive_snap_ = "B22003_002"
)
  
# Fetch data from census api
ACS_demographic_df_raw <- get_acs(
  geography = "county",
  variables = acs_demographic_vars,
  year = 2021,
  survey = "acs5",
  geometry = FALSE,
  output = "wide"
)

# Clean names
ACS_demographic_df_1 <- ACS_demographic_df_raw %>% 
  janitor::clean_names()

# Split geoid into state and county codes and separate state and county names
# Also drop Puerto Rico
ACS_demographic_df_2 <- ACS_demographic_df_1 %>% 
  separate(name, into = c("county_name", "state_name"), sep = ",\\s*") %>% 
  mutate(
    state_fips = substr(geoid, 1, 2),
    county_fips = substr(geoid, 3, 5)
  ) %>% 
  filter(state_fips != "72")

# Drop margin of error vars 
ACS_demographic_df_3 <- ACS_demographic_df_2 %>% 
  select(-ends_with("_m")) %>% 
  rename_with(~sub("_e$", "", .), ends_with("_e"))

# Calculate percentages for variables and create new df
ACS_demographic_df_4 <- ACS_demographic_df_3 %>%
  mutate(
    per_non_hisp_white = non_hisp_white / ethnicity_denom, 
    per_non_white = 1 - per_non_hisp_white,
    per_non_hisp_black = non_hisp_black / ethnicity_denom,
    per_non_hisp_native = non_hisp_native / ethnicity_denom, 
    per_non_hisp_asian = non_hisp_asian / ethnicity_denom, 
    per_non_hisp_pi = non_hisp_pi / ethnicity_denom,
    
    per_hisp = hisp_total / ethnicity_denom,                      
    per_hisp_white = hisp_white / ethnicity_denom,
    per_hisp_black = hisp_black / ethnicity_denom,
    per_hisp_native = hisp_native / ethnicity_denom,
    per_hisp_asian = hisp_asian / ethnicity_denom,
    per_hisp_pi = hisp_pi / ethnicity_denom,
    
    unemp_rate = unemployed / civilian_labor_force,
    per_disability = (disability_m_under5 + disability_m_5_17 + disability_m_18_34 + 
                        disability_m_35_64 + disability_m_65_74 + disability_m_75_over + 
                        disability_f_under5 + disability_f_5_17 + disability_f_18_34 + 
                        disability_f_35_64+ disability_f_65_74 + disability_f_75_over) / disability_total,
    
    per_h_owner = tenure_owner_occ / tenure_estimate_total,
    per_h_owner_white = tenure_owner_occ_white / tenure_total_white,
    per_h_owner_non_hisp_white = tenure_owner_occ_non_hisp_white / tenure_total_non_hisp_white,
    per_h_owner_black = tenure_owner_occ_black / tenure_total_black,
    per_h_owner_native = tenure_owner_occ_native / tenure_total_native,
    per_h_owner_asian = tenure_owner_occ_asian / tenure_total_asian,
    per_h_owner_pi = tenure_owner_occ_pi / tenure_total_pi,
    per_h_owner_hisp = tenure_owner_occ_hisp / tenure_total_hisp,
    
    per_below_fpl_overall = ifelse(fpl_est_tot > 0,
                                   fpl_below / fpl_est_tot, 0),
    per_below_fpl_white = if_else(fpl_est_tot_white > 0, 
                                  fpl_below_white / fpl_est_tot_white, 0),
    per_below_fpl_white_non_hisp = if_else(fpl_est_tot_white_non_hisp > 0, 
                                           fpl_below_white_non_hisp / fpl_est_tot_white_non_hisp, 0),
    per_below_fpl_black = if_else(fpl_est_tot_black > 0,
                                  fpl_below_black / fpl_est_tot_black, 0),
    per_below_fpl_native = if_else(fpl_est_tot_native > 0,
                                   fpl_below_native / fpl_est_tot_native, 0),
    per_below_fpl_asian = if_else(fpl_est_tot_asian > 0,
                                  fpl_below_asian / fpl_est_tot_asian, 0),
    per_below_fpl_pi = if_else(fpl_est_tot_pi > 0,
                               fpl_below_pi / fpl_est_tot_pi, 0),
    per_below_fpl_hisp = if_else(fpl_est_tot_hisp > 0,
                                 fpl_below_hisp / fpl_est_tot_hisp, 0),
    
    
    share_hh_receive_snap = num_hh_receive_snap / num_hh_snap_est
  )

# Save clean data as csv
ACS_demographic_df_clean <- ACS_demographic_df_4 %>% 
  select(geoid, state_name, county_name, state_fips, county_fips, total_pop, 
         median_hh_income,
         median_hh_income_non_hisp_white:median_hh_income_hisp,
         per_non_hisp_white:per_hisp,
         unemp_rate,
         per_disability,
         per_h_owner,
         per_h_owner_non_hisp_white:per_h_owner_hisp,
         per_below_fpl_white_non_hisp:per_below_fpl_hisp,
         share_hh_receive_snap
  ) %>% 
  write_csv(file = file.path("data", "modified-data", "ACS_demographic_clean.csv"))

####----2017 Census of Agriculture (COA)----####

COA_df_raw <- read.table(
  gzfile(file.path("data", "raw-data", "2017_cdqt_data.txt.gzip")),
  sep="\t"
) %>%
  row_to_names(row_number = 1)


COA_df_1 <- COA_df_raw %>%
  janitor::clean_names() %>%
  select(-census_column, -commodity_desc, -sector_desc) %>%
  rename(
    state_fips = state_fips_code,
    county_fips = county_code,
    state_abv = state_alpha
  )

var_desc_demographics <- c(
  "PRODUCERS, HISPANIC - NUMBER OF PRODUCERS",
  "PRODUCERS, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF PRODUCERS",
  "PRODUCERS, ASIAN - NUMBER OF PRODUCERS",
  "PRODUCERS, BLACK OR AFRICAN AMERICAN - NUMBER OF PRODUCERS",
  "PRODUCERS, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF PRODUCERS",
  "PRODUCERS, WHITE - NUMBER OF PRODUCERS",
  "PRODUCERS, MULTI-RACE - NUMBER OF PRODUCERS"
)

var_desc_commodity <- c(
  "COMMODITY TOTALS, INCL VALUE-ADDED, RETAIL, DIRECTLY MARKETED, HUMAN CONSUMPTION - SALES, MEASURED IN $",
  "COMMODITY TOTALS, INCL VALUE-ADDED, WHOLESALE, DIRECT TO RETAILERS & INSTITUTIONS & FOOD HUBS, LOCAL OR REGIONALLY BRANDED PRODUCTS, HUMAN CONSUMPTION - SALES, MEASURED IN $",
  "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES",
  "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES",
  "PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES"
)

var_desc_production <- c(
  "CORN - SALES, MEASURED IN $",
  "WHEAT - SALES, MEASURED IN $",
  "SOYBEANS - SALES, MEASURED IN $",
  "SORGHUM - SALES, MEASURED IN $",
  "BARLEY - SALES, MEASURED IN $",
  "RICE - SALES, MEASURED IN $",
  "GRAIN, OTHER - SALES, MEASURED IN $",
  "TOBACCO - SALES, MEASURED IN $",
  "COTTON, LINT & SEED - SALES, MEASURED IN $",
  "VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN - SALES, MEASURED IN $",
  "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $",
  "HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS) - SALES, MEASURED IN $",
  "CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS - SALES, MEASURED IN $",
  "FIELD CROPS, OTHER, INCL HAY - SALES, MEASURED IN $",
  "POULTRY TOTALS, INCL EGGS - SALES, MEASURED IN $",
  "CATTLE, INCL CALVES - SALES, MEASURED IN $",
  "MILK - SALES, MEASURED IN $",
  "HOGS - SALES, MEASURED IN $",
  "SHEEP & GOATS TOTALS, INCL WOOL & MOHAIR & MILK - SALES, MEASURED IN $",
  "EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $",
  "AQUACULTURE TOTALS - SALES & DISTRIBUTION, MEASURED IN $",
  "SPECIALTY ANIMAL TOTALS, (EXCL EQUINE) - SALES, MEASURED IN $"
)

COA_df_2 <- COA_df_1 %>%
  filter(agg_level_desc == "COUNTY",
         census_chapter == 2,
         census_table %in% list(2, 41, 48, 49, 50, 51, 52, 53, 54),
            (short_desc %in% var_desc_commodity |
            short_desc %in% var_desc_production |
              short_desc %in% var_desc_demographics)
  ) %>% 
  mutate(
    short_desc = case_when(
      short_desc == "PRODUCERS, HISPANIC - NUMBER OF PRODUCERS" ~
        "num_producers_hispanic",
      short_desc == "PRODUCERS, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF PRODUCERS" ~
        "num_producers_native",
      short_desc == "PRODUCERS, ASIAN - NUMBER OF PRODUCERS" ~
        "num_producers_asian",
      short_desc == "PRODUCERS, BLACK OR AFRICAN AMERICAN - NUMBER OF PRODUCERS" ~
        "num_producers_black",
      short_desc == "PRODUCERS, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF PRODUCERS" ~
        "num_producers_pi",
      short_desc == "PRODUCERS, WHITE - NUMBER OF PRODUCERS" ~
        "num_producers_white",
      short_desc == "PRODUCERS, MULTI-RACE - NUMBER OF PRODUCERS" ~
        "num_producers_other_race",
      short_desc == "COMMODITY TOTALS, INCL VALUE-ADDED, RETAIL, DIRECTLY MARKETED, HUMAN CONSUMPTION - SALES, MEASURED IN $" ~
        "commodity_total_direct",
      short_desc == "COMMODITY TOTALS, INCL VALUE-ADDED, WHOLESALE, DIRECT TO RETAILERS & INSTITUTIONS & FOOD HUBS, LOCAL OR REGIONALLY BRANDED PRODUCTS, HUMAN CONSUMPTION - SALES, MEASURED IN $" ~
        "commodity_total_retail",
      short_desc == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES" ~
        "conservation_no_till",
      short_desc == "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES" ~
        "conservation_excl_no_till",
      short_desc == "PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES" ~
        "conventional_till",
      short_desc == "CORN - SALES, MEASURED IN $" ~ "corn",
      short_desc == "WHEAT - SALES, MEASURED IN $" ~ "wheat_crop",
      short_desc == "SOYBEANS - SALES, MEASURED IN $" ~ "soybeans",
      short_desc == "SORGHUM - SALES, MEASURED IN $" ~ "sorghum",
      short_desc == "BARLEY - SALES, MEASURED IN $" ~ "barley",
      short_desc == "RICE - SALES, MEASURED IN $" ~ "rice",
      short_desc == "GRAIN, OTHER - SALES, MEASURED IN $" ~ "grain_other",
      short_desc == "TOBACCO - SALES, MEASURED IN $" ~ "tobacco",
      short_desc == "COTTON, LINT & SEED - SALES, MEASURED IN $" ~ "cotton",
      short_desc == "VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN - SALES, MEASURED IN $" ~ 
        "vegetables",
      short_desc == "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $" ~ "fruit",
      short_desc == "HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS) - SALES, MEASURED IN $" ~ 
        "horticulture",
      short_desc == "CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS - SALES, MEASURED IN $" ~ 
        "woody_crops",
      short_desc == "FIELD CROPS, OTHER, INCL HAY - SALES, MEASURED IN $" ~ 
        "hay",
      short_desc == "POULTRY TOTALS, INCL EGGS - SALES, MEASURED IN $" ~ 
        "chicken_and_eggs",
      short_desc == "CATTLE, INCL CALVES - SALES, MEASURED IN $" ~ 
        "cattle",
      short_desc == "MILK - SALES, MEASURED IN $" ~ "milk",
      short_desc == "HOGS - SALES, MEASURED IN $" ~ "pigs",
      short_desc == "SHEEP & GOATS TOTALS, INCL WOOL & MOHAIR & MILK - SALES, MEASURED IN $" ~ 
        "sheep",
      short_desc == "EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $" ~ 
        "horses",
      short_desc == "AQUACULTURE TOTALS - SALES & DISTRIBUTION, MEASURED IN $" ~ 
        "aquaculture",
      short_desc == "SPECIALTY ANIMAL TOTALS, (EXCL EQUINE) - SALES, MEASURED IN $" ~ 
        "specialty_animals"
    )
  )

# Character vector of id_col var names
id_vars <- c("state_fips", "state_name", "state_abv", "county_fips", "county_name")
name_vars <- c("short_desc")

# Pivot df to wide and clean variable names again
COA_df_wide <- COA_df_2 %>%
  pivot_wider(
    id_cols = all_of(id_vars),
    id_expand = F,
    names_from = short_desc,
    values_from = value
  ) %>%
  janitor::clean_names() %>% 
  mutate_at(vars(starts_with("producers")), as.numeric)

# Change missing values to zero across all variables (NASS confirmed that missing values are 0's). 
# Also change "(Z)" values to zero since these indicate very small values (less than half a unit) and change "(D)" values to missing since they are suppressed.
COA_df_wide_2 <- COA_df_wide %>% 
  mutate_all(~ifelse(is.na(.), "0", .)) %>% 
  mutate_all(~ifelse(trimws(.) == "(Z)", "0", .)) %>% 
  mutate_all(~ifelse(trimws(.) == "(D)", NA, .)) %>% 
  mutate_at(vars(corn:num_producers_other_race),
            ~parse_number(gsub(",", "", .)))

edible_crops_vars <- c("wheat_crop", "rice", "grain_other", "vegetables", 
                       "fruit")
non_edible_vars <- c("corn", "soybeans", "sorghum", "barley", "tobacco", 
                     "cotton", "horticulture", "woody_crops", "hay", "sheep", 
                     "horses", "aquaculture", "specialty_animals")
edible_animals <- c("chicken_and_eggs", "cattle", "milk", "pigs")

# Calculate vars of interest
COA_df_wide_3 <- COA_df_wide_2 %>% 
  mutate(
    num_producers = rowSums(across(starts_with("num_producers")), na.rm = TRUE),
    num_producers_aapi = num_producers_asian + num_producers_pi,
    share_producers_hispanic = num_producers_hispanic / num_producers, 
    share_producers_native = num_producers_native / num_producers, 
    share_producers_black = num_producers_black / num_producers, 
    share_producers_aapi = num_producers_aapi / num_producers,
    share_producers_white = num_producers_white / num_producers, 
    share_producers_other_race = num_producers_other_race / num_producers, 
    share_producers_nonwhite = share_producers_black + share_producers_hispanic + share_producers_native + share_producers_aapi + share_producers_other_race, 
    commodity_total = commodity_total_direct + commodity_total_retail,
    # setting value to NA when denominator is 0
    share_commodity_direct = case_when(
      commodity_total > 0 ~ commodity_total_direct / commodity_total,
      commodity_total == 0 ~ NA),
    edible_crops = rowSums(across(all_of(edible_crops_vars)), na.rm = TRUE),
    non_edible = rowSums(across(all_of(non_edible_vars)), na.rm = TRUE),
    edible_animals = rowSums(across(all_of(non_edible_vars)), na.rm = TRUE),
    production_total = edible_crops + non_edible + edible_animals,
    # setting value to NA when denominator is 0
    share_production_edible_crops = case_when(
      production_total > 0 ~ edible_crops / production_total,
      production_total == 0 ~ NA),
    share_production_non_edible = case_when(
      production_total > 0 ~ non_edible / production_total,
      production_total == 0 ~ NA),
    share_production_edible_animals = case_when(
      production_total > 0 ~ edible_animals / production_total,
      production_total == 0 ~ NA)
  )

# Remove un-needed vars and create geoid var
COA_df_wide_4 <- COA_df_wide_3 %>% 
  mutate(geoid = paste0(state_fips, county_fips))%>% 
  select(geoid, starts_with("share")) 

# Change 02010 geoid to 02013 and 02016 because counties changed between 2017 and 2021 (also done for FEA)
COA_df_wide_final <- COA_df_wide_4 %>% 
  mutate(geoid = if_else(geoid == "02010", "02013", geoid))

COA_df_02013 <- COA_df_wide_final %>% 
  filter(geoid == "02013") %>% 
  mutate(geoid = "02016")

# adding 02016 to main df
COA_df_wide_clean <- COA_df_wide_final %>% 
  bind_rows(COA_df_02013) %>% 
  # Save clean data as csv
  write_csv(file = file.path("data", "modified-data", "COA_clean.csv"))

####----Tribal Land Area----####


# Source of tribal data: 
tribal_lands <- native_areas(cb = TRUE)

all_counties <- counties(cb = TRUE)

# Calculate total area from imported data sets
all_counties_2 <- all_counties %>% 
  select(state_fips = STATEFP, state_abbv = STUSPS, county_fips = COUNTYFP, county_name = NAMELSAD, state_name = STATE_NAME) %>% 
  mutate(counties_area = st_area(.),
         county_fips = paste0(state_fips, county_fips)) %>% 
  filter(state_fips != 66 & state_fips != 72 & state_fips != 78 & state_fips != 69 & state_fips != 60) # Dropping US territories 

# Calculate percentage of counties comprised by tribal lands - NOTE: This intersection takes a significant amount of time depending on compute power

st_intersection_faster <- function(x, y, ...){
  intersected_rows <- st_intersects(x, y)
  intersected_indices <- rowSums(as.matrix(intersected_rows)) > 0
  x_lim <- x[intersected_indices, ]
  x_lim
}

joined <- st_intersection_faster(all_counties_2, tribal_lands)

joined_clean <- joined %>% 
  mutate(intersection_area = st_area(.)) %>% 
  st_drop_geometry() %>%
  select(county_fips, intersection_area)

# Merge back with all_counties file to get total tribal land area for each county
tribal_pct <- left_join(
  x = all_counties_2 %>% st_drop_geometry, 
  y = joined_clean, 
  by = "county_fips") %>% 
  replace_na(list(intersection_area = units::set_units(0, "m^2"))) %>% 
  summarize(intersection_area = sum(intersection_area), .by = county_fips)

# Merge back to all_counties to get coverage percent
tribal_pct_final <- left_join(all_counties_2, tribal_pct, by='county_fips') %>% 
  mutate(tribal_percent = as.numeric(intersection_area / counties_area)) %>% 
  select(geoid = county_fips, tribal_percent) %>% 
  st_drop_geometry()

# Export Excel 
write.csv(tribal_pct_final, file.path("data", "modified-data", "tribal_land_clean.csv"), row.names = FALSE)

####----CDC National Environmental Public Health Tracking Network----####

precip_days <-  read_csv(file.path("data", "raw-data", "cdc_precip_2019.csv")) %>%
  rename(county_fips = CountyFIPS, ex_precip_days = Value) %>% 
  select(county_fips, ex_precip_days) %>% 
  mutate(share_ex_precip_days = (ex_precip_days / 365)*100)

heat_days <-  read_csv(file.path("data", "raw-data", "cdc_heat_days_2019.csv")) %>% 
  rename(county_fips = CountyFIPS, ex_heat_days = Value) %>% 
  select(county_fips, ex_heat_days) %>% 
  mutate(share_ex_heat_days = (ex_heat_days / 365)*100)

# Join CDC data
cdc_phys_res <- left_join(x = heat_days,
                          y = precip_days, 
                          by = "county_fips")

# Export 
cdc_phys_res_2 <- cdc_phys_res %>% 
  rename(geoid = county_fips) %>% 
  # Updating census geoid change 
  mutate(geoid = if_else(geoid == "46113", "46102", geoid))

write.csv(cdc_phys_res_2, file.path("data", "modified-data", "cdc_physres_clean.csv"), row.names = FALSE)

#####----Mobility Metrics----####

# Reading in data
mobility_long <- read_csv(file.path("data", "raw-data", "00_mobility-metrics_longitudinal.csv"))
mobility_raceeth <- read_csv(file.path("data", "raw-data", "01_mobility-metrics_race-ethnicity_longitudinal.csv"))
mobility_povexp <- read_csv(file.path("data", "raw-data", "02_poverty-exposure_race-ethnicity.csv"))

# keep 2021 data for share_employed from race/eth dataset
mobility_raceeth_small <- mobility_raceeth %>%
  filter(year == 2021) %>%
  select(year, state, county, state_name, county_name, population, subgroup, share_employed, share_employed_quality) 
  
# Poverty Exposure Data
mobility_povexp_small <- mobility_povexp %>% 
  filter(year == 2021) %>%
  select(year, state, county, state_name, county_name, subgroup, share_poverty_exposure, share_poverty_exposure_quality) 

# County Longitudinal Data
# keep 2021 data for housing wealth by race, jobs with a living wage, exposure to other races 
mobility_long_small_21 <- mobility_long %>%
  filter(year == 2021) %>%
  select(year, state, county, state_name, county_name, ratio_black_nh_house_value_households, 
         ratio_average_to_living_wage_quality, ratio_hispanic_house_value_households, ratio_hispanic_house_value_households_quality, 
         ratio_white_nh_house_value_households, ratio_white_nh_house_value_households_quality, ratio_other_nh_house_value_households, 
         ratio_other_nh_house_value_households_quality, share_black_nh_exposure, share_black_nh_exposure_quality, share_hispanic_exposure, 
         share_hispanic_exposure_quality, share_white_nh_exposure, share_white_nh_exposure_quality, share_other_nh_exposure, share_other_nh_exposure_quality, 
         ratio_average_to_living_wage, ratio_average_to_living_wage_quality) 

# function to reshape ratio variables 
calculate_ratio <- function(var) {
  x <- as.numeric(sub("^(\\d+\\.?\\d*)%:(\\d+\\.?\\d*)%", "\\1", var))
  y <- as.numeric(sub("^(\\d+\\.?\\d*)%:(\\d+\\.?\\d*)%", "\\2", var))
  ratio <- x / y
  return(ratio)
}

mobility_long_small_21 <- mobility_long_small_21 %>% 
  mutate(
    ratio_black_nh_house_value_households = if_else(
      (state=="23" & (county=="003" | county=="029")), 
      "0.0%:0.0%",
      ratio_black_nh_house_value_households)
  ) %>% 
  mutate(
    ratio_black_nh_house_value_households =
      calculate_ratio(ratio_black_nh_house_value_households),
    ratio_hispanic_house_value_households =
      calculate_ratio(ratio_hispanic_house_value_households),
    ratio_white_nh_house_value_households =
      calculate_ratio(ratio_white_nh_house_value_households),
    ratio_other_nh_house_value_households =
      calculate_ratio(ratio_other_nh_house_value_households)
  )


### Merge all Extracts Together
mobility_povexp_small_shape <- mobility_povexp_small %>%
  pivot_wider(names_from = subgroup,
              values_from = c(share_poverty_exposure, 
                              share_poverty_exposure_quality)) %>%
  janitor::clean_names() 

mobility_raceeth_small_shape <- mobility_raceeth_small %>%
  pivot_wider(names_from = subgroup,
              values_from = c(share_employed, share_employed_quality)) %>%
  select(-population) %>% 
  janitor::clean_names()

# Merge data together

# Datasets: 1) mobility_povexp_small_shape, 2) mobility_long_small_21, 3) mobility_raceeth_small_shape
mobility_metrics_full <- 
  tidylog::full_join(mobility_long_small_21, mobility_povexp_small_shape,
            by = c("county", "state", "state_name", "county_name", "year")) %>%
  select(-c(year)) %>% 
  full_join(mobility_raceeth_small_shape,
            by = c("county", "state", "state_name", "county_name")) %>%
  select(-c(year))

# select only variables for analysis
mobility_metrics_final <- mobility_metrics_full %>%
  select(-contains("_quality")) %>% 
  mutate (geoid = paste0(state, county)) %>% 
  select(-state, -county, -state_name, -county_name)

# save out final mobility metrics df
write.csv(mobility_metrics_final, file.path("data", "modified-data", "mobility_metrics_clean.csv"), row.names = FALSE)



