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


##-Bring in shapefiles
geoms <- bring_in_sfs()
ca_cnty_pnts = geoms$ca_cnty_pnts
ca_cnty_sf = geoms$ca_cnty_sf
hor_pnts = geoms$hor_pnts
hor_sf = geoms$hor_sf
