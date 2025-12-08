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


##-- color palettes and objects for maps and charts

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

rnk_pal <- c(
  "#1e0c47",
  "#854d88",
  "#f1f0ea"
)



m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 20
)


##################################
## - demographic rates by county 
##--(excludes statewide data)
dem_df  <- all_rates_dem_adj %>% 
  filter(geo_level == "county", group_var != "All") %>%
  select(county, group_var, group_var_cat, inf_rate_100k, sev_rate_100k) %>%
  group_by(county, group_var) %>%
  mutate(
    hgst_rt  = max(sev_rate_100k, na.rm = TRUE),
    high_grp = if_else(sev_rate_100k >= hgst_rt, "yes", "no")
  ) %>%
  ungroup() %>%
  arrange(group_var, group_var_cat)


dem_choices <- c(
  "Age Group"      = "age_cat",
  "Race/Ethnicity" = "race_short",
  "Sex"            = "sex"
)




cnty_ranked_df <- cnty_ranked_df %>%
  mutate(
    county_label = case_when(
      priority_tier == "Top Priority" ~ 
        glue("<span style='font-weight:bold; font-size:12px; color:#52176b'>{county}</span>"),
      
      priority_tier == "Second Priority" ~ 
        glue("<span style='font-weight:bold; font-size:12px; color:#854d88'>{county}</span>"),
      
      TRUE  ~ 
        glue("<span style='font-size:8px; color:#555555'>{county}</span>")),
    
    priority_tier = factor(
      priority_tier,
      levels = c("Top Priority", "Second Priority", "Third Priority")),
    pop_prop = 100 * pop_prop,
    
    county = forcats::fct_reorder(county, adj_sev_100k)
  ) 


cnty_ranked_plot_df <- cnty_ranked_df %>%
  mutate(
    priority_tier = factor(
      priority_tier,
      levels = c("Top Priority", "Second Priority", "Third Priority")
    ),
    county = forcats::fct_reorder(county, adj_sev_100k, .desc = TRUE)
  ) %>%
  mutate(
    county_chr   = as.character(county),
    priority_chr = as.character(priority_tier),
    county_label_html = dplyr::case_when(
      priority_chr == "Top Priority"   ~ glue::glue("<b>{county_chr}</b>"),
      priority_chr == "Second Priority" ~ glue::glue("<b>{county_chr}</b>"),
      TRUE                             ~ county_chr
    ),
  
    hover_lbl = glue(
      "<b>{county}</b>
       Severe AAR: <b>{scales::comma(round(adj_sev_100k, 1))}</b>"
    ) %>% as.character()
  )

tick_labels <- cnty_ranked_plot_df %>%
  dplyr::distinct(county, county_label_html) %>%
  dplyr::arrange(county) 




#########################
## - Bring in shapefiles
geoms <- bring_in_sfs()

hor_sf   <- geoms$hor_sf %>% select(hor)


cnty_sf <- geoms$ca_cnty_sf %>%
  select(county) %>%
  left_join(cnty_ranked_df, by = "county") %>%
  
  mutate(
    priority_tier = factor(
      priority_tier,
      levels = c("Top Priority", "Second Priority", "Third Priority")
    ), 
    
    hover_lbl = glue(
      "<strong>{county}</strong><br>
       Severe AAR: <strong>{scales::comma(round(adj_sev_100k, 1))}</strong>"
    ) %>% as.character()
  )


cnty_centroids <- cnty_sf %>%
  st_centroid() 

cnty_dorl <- cnty_sf %>%
  st_transform(3310) %>%
  mutate(dorl_wt = 0.8) %>% 
  cartogram_dorling(
    weight  = "dorl_wt",
    k       = 0.2,  
    itermax = 200
  )

cnty_dorl_centroids <- cnty_dorl %>%
  st_centroid()

cnty_dorl_centroids <- cnty_dorl_centroids %>%
  st_transform(4326) %>%
  mutate(
    lng = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2],
    radius = scales::rescale(adj_sev_100k, to = c(3, 15))
  )

# palette
pal_priority <- colorFactor(
  palette = c(
    "Top Priority"            = "#1e0c47",
    "Second Priority"         = "#854d88",
    "Third/Fourth Priority"   = "#f1f0ea"
  ),
  domain  = cnty_dorl_centroids$priority_tier
)

