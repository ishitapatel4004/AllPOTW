setwd("C:/Users/TWIntern/OneDrive - Team Car Wash/Desktop/AllPOTW")

# ---- Load Required Libraries ----
library(shiny)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(ggplot2)
library(DT)
library(plotly)
library(knitr)
library(readxl)
library(kableExtra)
library(stringr)

# ---- Load POTW Campaign Files ----
load_campaign <- function(file, campaign_name) {
  read_csv(file, show_col_types = FALSE) %>%
    rename(
      CREATED_DATE = CREATED_DATE,
      LOCATION = LOCATION_NAME,
      TOTAL_WASHES = TOTAL_WASHES,
      FREE_WASHES = FREE_WASHES,
      REDEEMED_WASHES = REDEEMED_WASHES,
      ELIGIBLE_WASHES = ELIGIBLE_WASHES,
      SALES = TOTAL_SALES,
      Conversions = SALES_PER_ELIGIBLE_WASH
    ) %>%
    mutate(
      `CREATED DATE` = as.Date(CREATED_DATE),
      Campaign = campaign_name
    ) %>%
    select(`CREATED DATE`, LOCATION, TOTAL_WASHES, FREE_WASHES,
           REDEEMED_WASHES, ELIGIBLE_WASHES, SALES, Conversions, Campaign)
}

# ---- Load Each POTW Campaign CSV ----
POTW_Spring2023 <- load_campaign("POTWSpring2023.csv", "POTW Spring 2023")
POTW_Spring2024 <- load_campaign("POTWSpring2024.csv", "POTW Spring 2024")
POTW_Spring2025 <- load_campaign("POTWSpring2025.csv", "POTW Spring 2025")
POTW_Winter2023 <- load_campaign("POTWWinter2023.csv", "POTW Winter 2023")
POTW_Winter2024 <- load_campaign("POTWWinter2024.csv", "POTW Winter 2024")
POTW_Winter2025 <- load_campaign("POTWWinter2025.csv", "POTW Winter 2025")

# ---- Combine Campaigns ----
POTW_All_Campaigns <- bind_rows(
  POTW_Spring2023, POTW_Spring2024, POTW_Spring2025,
  POTW_Winter2023, POTW_Winter2024, POTW_Winter2025
)

# ---- Standardize LOCATION ----
standardize_locations <- function(df) {
  df %>%
    mutate(LOCATION = str_replace_all(LOCATION, c(
      "Monmouth Junct"       = "Monmouth Junction",
      "Monmouth Junctionion" = "Monmouth Junction",
      "Roselle Pk"           = "Roselle Park",
      "Tices Ln"             = "Tices Lane",
      "Tices Lne"            = "Tices Lane",
      "East brunswick"       = "East Brunswick",
      "east brunswick"       = "East Brunswick",
      "East Brunswick 18"    = "East Brunswick"
    )))
}

POTW_All_Campaigns <- standardize_locations(POTW_All_Campaigns)

# ---- Add Date Metadata ----
POTW_All_Campaigns <- POTW_All_Campaigns %>%
  mutate(
    Year = year(`CREATED DATE`),
    Session = if_else(month(`CREATED DATE`) %in% 1:3, "Winter", "Spring")
  )

print("✅ COLUMN NAMES IN POTW_All_Campaigns:")
print(colnames(POTW_All_Campaigns))

# ---- Load and Clean LTV File ----
LTV_by_Site <- read_csv("LTV by site2.csv", 
                        col_types = cols(.default = col_guess())) %>%
  rename_with(~ gsub("^X\\.", "", .x)) %>%
  standardize_locations() %>%
  filter(!is.na(LOCATION))

# ---- Define Locations ----
available_locations <- c(
  "Piscataway", "Westfield", "Summit", "Woodbridge",   # Full Service
  "Marlboro", "Roselle", "Monmouth Junction", "East Brunswick",
  "Oakhurst", "Hazlet", "Tices Lane", "Scotch Plains", "Roselle Park"  # Express
)

full_service_locations <- c("Piscataway", "Westfield", "Summit", "Woodbridge")
express_locations <- setdiff(available_locations, full_service_locations)
