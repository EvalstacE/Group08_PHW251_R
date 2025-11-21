# Load and install packages

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(janitor)
library(lubridate)
library(rlang)
library(stringr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(MMWRweek)
library(shiny)
library(bslib)
library(rsconnect)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# Bring in helper functions
func_dir <- here::here("functions")
r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
purrr::walk(r_files, source)


# Bring in datasets
data_dirs <- c(
  here::here("data", "tbl_outputs_03"),
  here::here("data", "cleaned_data")
)

csv_files <- purrr::map(data_dirs, ~ list.files(
  path = .x,
  pattern = "\\.csv$",
  full.names = TRUE
)) %>% unlist()

purrr::walk(csv_files, function(file_path) {
  obj_name <- tools::file_path_sans_ext(basename(file_path))
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  assign(obj_name, df, envir = knitr::knit_global())
})


## - Bring in shapefiles
geoms <- bring_in_sfs()

ca_cnty_sf <- geoms$ca_cnty_sf %>%
  st_make_valid() %>%
  left_join(inf_rates_by_cnty, by = "county") %>%
  select(county, total_infected) %>%
  rename("cumulative_iinfected" = "total_infected")




# one point per county, with lon/lat columns
ca_cnty_pnts <- ca_cnty_sf %>%
  st_centroid() %>%              
  mutate(
    lng = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(county, lng, lat)

hor_pnts <- geoms$hor_pnts
hor_sf   <- geoms$hor_sf

## -- weekly case data
cnty_weekly_cases <- combined_df %>%
  group_by(
    health_officer_region,
    county,
    mmwr_week,
    start_date,
    end_date
  ) %>%
  summarise(
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
    total_cnty_pop = first(total_cnty_pop), 
    inf_rate_100k = round((10^4 * cumulative_infected / total_cnty_pop),1),
    .groups = "drop"
  )

summary(cnty_weekly_cases$inf_rate_100k)

## -- make weekly pnt data (no geometry, just lng/lat)
cnty_week_pnts <- cnty_weekly_cases %>%
  left_join(ca_cnty_pnts, by = "county")


