library(pacman)
pacman::p_load(
  dplyr, tidyr, readr, here, janitor, lubridate,  
  rlang, stringr, purrr, ggplot2,  MMWRweek
)

combined_df <- read.csv(file = here("data/cleaned_data/combined_df.csv"))

rates_by_dem_df <- read.csv(file = here("data/cleaned_data/all_rates_by_demographic.csv"))
