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
library(leaflet)
library(scales)
library(htmltools)
library(htmlwidgets)
library(jsonlite)
library(bsicons)

options(tigris_use_cache = TRUE)

# Bring in helper functions
func_dir <- here::here("functions")
r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
purrr::walk(r_files, source)


# Bring in datasets
data_dirs <- c(
  here::here("data", "inf_rate_dfs"),
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

ca_cnty_pnts <- geoms$ca_cnty_pnts
hor_sf   <- geoms$hor_sf
hor_pnts <- geoms$hor_pnts
cnty_week_pnts <- read.csv(file = here("data/shapefiles/cnty_week_pnts.csv"))
hor_week_pnts <- read.csv(file = here("data/shapefiles/hor_week_pnts.csv"))

ca_cnty_sf <- geoms$ca_cnty_sf %>%
  left_join(inf_rates_by_cnty, by = "county")

top_cnty_rates <- ca_cnty_sf %>%
  arrange(desc(inf_rate_100k)) %>%
  slice_head(n = 5) %>%
  bind_rows(
    ca_cnty_sf %>%
      arrange(desc(sev_rate_100k)) %>%
      slice_head(n = 5)
  ) %>%
  distinct()

break_cols <- c("case_breaks", "inf_rate_breaks",
                "sev_case_breaks", "sev_rate_breaks")

cnty_week_pnts <- cnty_week_pnts %>%
  mutate(across(all_of(break_cols), enforce_factor_levels))

hor_week_pnts <- hor_week_pnts %>%
  mutate(across(all_of(break_cols), enforce_factor_levels))

  

## - color palettes for maps

cnty_rate_levels <- levels(cnty_week_pnts$inf_rate_breaks)
cnty_sev_rate_levels <- levels(cnty_week_pnts$sev_rate_breaks)
cnty_case_levels <- levels(cnty_week_pnts$case_breaks)


custom_pal_cases <- colorFactor(
  palette = c(
    "#fdacb8",
    "#b93f76",
    "#892a68",
    "#52176b",
    "#1e0c47"
  ),
  domain  = cnty_rate_levels, 
  ordered = TRUE
)


custom_pal_sev_cases <- colorFactor(
  palette = c(
    "#fdacb8",
    "#b93f76",
    "#892a68",
    "#52176b",
    "#1e0c47"
  ),
  domain  = cnty_sev_rate_levels, 
  ordered = TRUE
)


## -- scale infections + severe infections proportionally 

sev_scale <- 
  max(cnty_week_pnts$inf_rate_100k, na.rm = TRUE) /
  max(cnty_week_pnts$sev_rate_100k, na.rm = TRUE)

