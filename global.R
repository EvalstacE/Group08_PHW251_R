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
library(classInt)
library(RColorBrewer)
library(viridis)
library(viridisLite)
library(rcartocolor)
library(DT)
library(magrittr)
library(plotly)
library(glue)



options(tigris_use_cache = TRUE)

# Bring in helper functions
func_dir <- here::here("functions")
r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
purrr::walk(r_files, source)


# Bring in datasets
data_dirs <- c(
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


## - dataframes to work with
all_rates_df <- all_rates_by_demographic
dem_df       <- all_rates_by_demographic %>% filter(geo_level != "statewide", group_var != "All")
df           <- combined_df

cnty_rates_df <- all_rates_df %>%
  filter(geo_level == "county", group_var == "All") %>%
  select(county, cumulative_infected, inf_rate_100k, cumulative_severe, sev_rate_100k, group_pop, total_ca_pop)%>%
  mutate(pop_prop = 100*group_pop / total_ca_pop) %>%
  create_EQ_lbl(inf_rate_100k) %>%
  create_EQ_lbl(sev_rate_100k) 


top_cnty_rates <- cnty_rates_df %>%
  filter(
    inf_rate_100k > 23233 |
    sev_rate_100k > 598
  ) %>%
  distinct()


top_sev_df <- cnty_rates_df %>%
  filter(sev_rate_100k > 598) %>%
  distinct()




hor_rates_df <- all_rates_df %>%
  filter(geo_level == "region", group_var == "All") %>%
  select(health_officer_region, inf_rate_100k, sev_rate_100k)


## - Bring in shapefiles
geoms <- bring_in_sfs()

cnty_sf <- geoms$ca_cnty_sf %>%
  select(county) %>%
  left_join(cnty_rates_df, by = "county") %>%
  mutate(
    hover_lbl = glue(
      "<strong>{county}</strong><br>
       Rate: <strong>{round(sev_rate_100k, 1)}</strong>"
    ) %>% as.character()
  )

cnty_pnts <- geoms$ca_cnty_pnts %>%
  left_join(cnty_rates_df, by = "county")%>%
  dplyr::mutate(
    sev_rate_100k = ifelse(is.na(sev_rate_100k), 0, sev_rate_100k),
    radius = scales::rescale(sev_rate_100k, to = c(3, 15))
  ) 


hor_pnts <- geoms$hor_pnts %>% rename("health_officer_region" = "hlth_f_") %>%
  left_join(hor_rates_df, by = "health_officer_region")%>%
  dplyr::mutate(
    sev_rate_100k = ifelse(is.na(sev_rate_100k), 0, sev_rate_100k)
  ) 


hor_sf   <- geoms$hor_sf %>% select(hlth_f_) %>% rename("health_officer_region" = "hlth_f_") %>%
  left_join(hor_rates_df, by = "health_officer_region")





##-- color palettes for maps

lgt_clr <- "#fcebed"
drk_clr <- "#1e0c47"
drkst_clr <- "#0f172a"
grid_clr <- "#212738"


custom_pal <- c(
  "#fcebed",
  "#fdacb8",
  "#b93f76",
  "#52176b",
  "#1e0c47"
)


cnty_pal <- colorNumeric(
  palette = custom_pal, 
  domain  = cnty_pnts$sev_rate_100k
)

hor_pal <- colorNumeric(
  palette = custom_pal,
  domain = hor_pnts$sev_rate_100k
)

m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 20
)
