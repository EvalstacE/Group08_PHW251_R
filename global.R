# Load and install packages

library(shiny)
library(bslib)
library(rsconnect)
library(htmltools)
library(htmlwidgets)
library(jsonlite)
library(bsicons)

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(janitor)
library(lubridate)
library(rlang)
library(stringr)
library(glue)
library(scales)
library(purrr)
library(MMWRweek)
library(classInt)
library(magrittr)

library(ggplot2)
library(ggthemes)
library(plotly)

library(DT)
library(RColorBrewer)
library(viridis)
library(viridisLite)

library(leaflet)
library(cartogram)
library(sf)


# Bring in helper functions
func_dir <- here::here("functions")
r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
purrr::walk(r_files, source)


# Bring in datasets
data_dirs <- c(here::here("data", "cleaned_data"))

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

cnty_sf  <- geoms$ca_cnty_sf
hor_sf   <- geoms$hor_sf

# county centroids (center points)
cnty_centroids <- cnty_sf %>%
  st_transform(3310) %>%      
  st_centroid() %>%
  st_transform(4326) %>%      
  select(county)            



weekly_df <- combined_df %>%
  group_by(mmwr_week, start_date, end_date, county) %>%
  summarise(
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE), 
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    new_cases           = sum(new_infections, na.rm = TRUE),
    .groups = "drop"
  )

max_cases     <- max(weekly_df$cumulative_infected, na.rm = TRUE)
max_sev_cases <- max(weekly_df$cumulative_severe, na.rm = TRUE)


## - cumulative cases line chart

p1_df <- weekly_df %>% 
  group_by(mmwr_week, start_date, end_date) %>%
  summarise(
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE), 
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    new_cases           = sum(new_cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mmwr_week) %>%
mutate(
    p_chng = (cumulative_infected - lag(cumulative_infected)) / lag(cumulative_infected) * 100,
    s_dt   = format(as_date(start_date), format = "%b %d"),
    e_dt   = format(as_date(end_date),   format = "%b %d"),
    cum_lbl = scales::comma(cumulative_infected, big.mark = ","),
    new_lbl = scales::comma(new_cases, big.mark = ","),
    p_chng_scaled = case_when(
      is.na(p_chng) ~ 4,
      max(p_chng, na.rm = TRUE) == min(p_chng, na.rm = TRUE) ~ 8,
      TRUE ~ scales::rescale(
        p_chng,
        to   = c(4, 14),
        from = range(p_chng, na.rm = TRUE)
      )
    )
)

max_chng <- max(p1_df$p_chng, na.rm = TRUE)



##-- color palettes and objects for maps and charts

lgt_clr <- "#fcebed"
drk_clr <- "#1e0c47"
drkst_clr <- "#0f172a"
grid_clr <- "#212738"

cat_pal <- 
  c("#31888d", "#52176b", "#f6c143", "#ff66c4", "#61c46e", "#395b8b", "#c1434e")


custom_pal <- c(
  "#fcebed",
  "#fdacb8",
  "#b93f76",
  "#52176b",
  "#1e0c47"
)

rnk_pal <- c(
  "#1e0c47",
  "#854d88",
  "#f1f0ea"
)

