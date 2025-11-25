# Load and install packages
pacman::p_load(
  dplyr, tidyr, readr, here, janitor, lubridate,  
  rlang, stringr, purrr, ggplot2, ggthemes, knitr,
  kableExtra, MMWRweek, scales, sf, tigris, classInt,
  plotly, leaflet, ggspatial, glue, htmltools, htmlwidgets,
  ggtext, ggfx, grid, forcats, gganimate, cowplot, ggrepel,
  reactable, RColorBrewer
)

options(
  tigris_use_cache = TRUE,
  scipen           = 9999
)

# Bring in helper functions
func_dir <- here::here("functions")
r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
purrr::walk(r_files, source)


# Bring in datasets
data_dirs <- c(
  here::here("data", "cleaned_data"),
  here::here("data", "inf_rate_dfs")
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


inf_rates_by_cnty <- inf_rates_by_cnty %>%
  rename("cumulative_infected" = "total_infected",
         "cumulative_severe" = "total_severe")

inf_rates_by_HOR <- inf_rates_by_HOR %>%
  rename("cumulative_infected" = "total_infected",
         "cumulative_severe" = "total_severe")


source("_common04_maps.R")