setwd("C:/Users/TWIntern/OneDrive - Team Car Wash/Desktop/AllPOTW")

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

# ---- Load POTW Campaign Files ----
load_campaign <- function(file, campaign_name) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      `CREATED DATE` = as.Date(`CREATED DATE`, format = "%m/%d/%Y"),
      Campaign = campaign_name
    )
}

# ---- Load All Campaigns ----
POTW_Spring2023 <- load_campaign("POTWSpring2023.csv", "POTW Spring 2023")
POTW_Spring2024 <- load_campaign("POTWSpring2024.csv", "POTW Spring 2024")
POTW_Spring2025 <- load_campaign("POTWSpring2025.csv", "POTW Spring 2025")
POTW_Winter2023 <- load_campaign("POTWWinter2023.csv", "POTW Winter 2023")
POTW_Winter2024 <- load_campaign("POTWWinter2024.csv", "POTW Winter 2024")
POTW_Winter2025 <- load_campaign("POTWWinter2025.csv", "POTW Winter 2025")

# ---- Combine All ----
POTW_All_Campaigns <- bind_rows(
  POTW_Spring2023, POTW_Spring2024, POTW_Spring2025,
  POTW_Winter2023, POTW_Winter2024, POTW_Winter2025
)

POTW_All_Campaigns <- POTW_All_Campaigns %>%
  mutate(
    Year = year(`CREATED DATE`),
    Session = if_else(month(`CREATED DATE`) %in% 1:3, "Winter", "Spring")
  )

print("✅ COLUMN NAMES IN POTW_All_Campaigns:")
print(colnames(POTW_All_Campaigns))

# ---- Load LTV by Site ----
LTV_by_Site <- read_csv("LTV by site2.csv", show_col_types = FALSE)

# ---- Define Locations ----
available_locations <- c(
  "Piscataway", "Westfield", "Summit", "Woodbridge",     # Full Service
  "Marlboro", "Roselle", "Monmouth Junction", "East Brunswick",
  "Oakhurst", "Hazlet", "Tices Lane", "Scotch Plains", "Roselle Park"   # Express
)

full_service_locations <- c("Piscataway", "Westfield", "Summit", "Woodbridge")
express_locations <- setdiff(available_locations, full_service_locations)
