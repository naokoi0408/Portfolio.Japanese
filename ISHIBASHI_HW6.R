#----------------------------------------------
# DATA 101
# Homework 6: Naoko Ishibashi
#----------------------------------------------
## Load in the libraries
library(maps)
library(mapdata)
library(tidycensus) # To access Census data
library(sf)         # For spatial data
library(tigris)     # For shapefiles
library(ggplot2)    # For visualization
library(dplyr)      # For data wrangling
library(tidyverse)
library(sf)
library(viridis)
library(rsconnect)

# install.packages('leaflet')


# GEOID: Unique identifier for geographic areas.
# NAME: Descriptive name of the tract.
# estimate: Median Household Income estimate.
# geometry: Spatial data for mapping.

census_api_key("eef9065461b49f7b29cb6e9455903e624410b03d", install = TRUE, overwrite = TRUE)

# Load all variables from ACS 5-Year 2021 dataset
variables <- load_variables(2021, "acs5", cache = TRUE)

# Filter for the B01001 table (Sex by Age)
age_variables <- variables[grep("B01001", variables$name), ]
View(age_variables)  # View the full list in a table format

# Load the necessary variables age over 65 Total
philly_data <- get_acs(
  geography = "tract",
  variables = c(
    age_M_65_66 = "B01001_020",  # Male, age 65 & 66
    age_M_67_69 = "B01001_021",  # Male, age 67-69
    age_M_70_74 = "B01001_022",  # Male, age 70-74
    age_M_75_79 = "B01001_023",  # Male, age 75-79
    age_M_80_84 = "B01001_024",  # Male, age 80-84
    age_M_85_plus = "B01001_025", # Male, age 85+
    
    age_F_65_66 = "B01001_044",  # Female, age 65 & 66
    age_F_67_69 = "B01001_045",  # Female, age 67-69
    age_F_70_74 = "B01001_046",  # Female, age 70-74
    age_F_75_79 = "B01001_047",  # Female, age 80-84
    age_F_80_84 = "B01001_048",  # Female, age 80-84
    age_F_85_plus = "B01001_049" # Female, age 85+
    
  ),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)

# Summing age groups 65+
philly_data_65_plus <- philly_data %>%
  group_by(GEOID, NAME) %>%
  summarise(age_65_plus = sum(estimate, na.rm = TRUE)) %>%
  ungroup()

# Plot the data
ggplot(philly_data_65_plus) +
  geom_sf(aes(fill = age_65_plus), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Population Aged 65 and Older in Philadelphia",
    fill = "Population 65+"
  )

ggsave("population_65_plus_map.png", width = 10, height = 8)


# Load the necessary variables age over 65 Male ####
philly_data <- get_acs(
  geography = "tract",
  variables = c(
    
    age_M_65_66 = "B01001_020",  # Male, age 65 & 66
    age_M_67_69 = "B01001_021",  # Male, age 67-69
    age_M_70_74 = "B01001_022",  # Male, age 70-74
    age_M_75_79 = "B01001_023",  # Male, age 75-79
    age_M_80_84 = "B01001_024",  # Male, age 80-84
    age_M_85_plus = "B01001_025", # Male, age 85+
    
  ),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)

# Summing age groups 65+
philly_data_65_plus <- philly_data %>%
  group_by(GEOID, NAME) %>%
  summarise(age_65_plus = sum(estimate, na.rm = TRUE)) %>%
  ungroup()

# Plot the data
ggplot(philly_data_65_plus) +
  geom_sf(aes(fill = age_65_plus), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Population Aged 65 and Older in Philadelphia",
    fill = "Population 65+"
  )

ggsave("population_65_plus_map.png", width = 10, height = 8)

# Load the necessary variables age over 65 Female ###
philly_data <- get_acs(
  geography = "tract",
  variables = c(
    
    age_F_65_66 = "B01001_044",  # Female, age 65 & 66
    age_F_67_69 = "B01001_045",  # Female, age 67-69
    age_F_70_74 = "B01001_046",  # Female, age 70-74
    age_F_75_79 = "B01001_047",  # Female, age 80-84
    age_F_80_84 = "B01001_048",  # Female, age 80-84
    age_F_85_plus = "B01001_049" # Female, age 85+
    
  ),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)

# Summing age groups 65+
philly_data_65_plus <- philly_data %>%
  group_by(GEOID, NAME) %>%
  summarise(age_65_plus = sum(estimate, na.rm = TRUE)) %>%
  ungroup()

# Plot the data
ggplot(philly_data_65_plus) +
  geom_sf(aes(fill = age_65_plus), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Population Aged 65 and Older in Philadelphia",
    fill = "Population 65+"
  )

ggsave("population_65_plus_map.png", width = 10, height = 8)





# Fetch data for Median Household Income ---------------------------------------
income_data <- get_acs(
  geography = "tract",
  variables = c(median_income = "B19013_001"),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)

head(income_data)

ggplot(income_data) +
  geom_sf(aes(fill = estimate), color = NA) +
  # scale_fill_viridis_c(option = "cividis") +
  scale_fill_viridis_c(option = "cividis") +
  theme_minimal() +
  labs(
    title = "Median Household Income in Philadelphia",
    fill = "Median Income ($)"
  )

ggsave("median_household_income_map.png", width = 10, height = 8)

#-------------------------------------------------------------------------------