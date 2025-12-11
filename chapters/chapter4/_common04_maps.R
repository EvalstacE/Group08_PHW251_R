
###- '_common04.R' runs first (brings in all functions)
#### -- then sources this file 



weekly_df <- combined_df %>%
  group_by(mmwr_week, start_date, end_date, county) %>%
  summarise(
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE), 
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    new_cases           = sum(new_infections, na.rm = TRUE),
    .groups = "drop"
  )

##-- Bring in shapefiles


geoms <- bring_in_sfs()


cnty_sf <- geoms$ca_cnty_sf %>%
  left_join(weekly_df %>% filter(mmwr_week == 52), by = "county")



hor_sf <- geoms$hor_sf 
state_sf <- geoms$ca_state_sf


# county centroids (center points)
cnty_centroids <- cnty_sf %>%
  st_transform(3310) %>%      
  st_centroid() %>%
  st_transform(4326) %>%      
  select(county)    




max_cases     <- max(weekly_df$cumulative_infected, na.rm = TRUE)
max_sev_cases <- max(weekly_df$cumulative_severe, na.rm = TRUE)

p95 <- quantile(cnty_sf$cumulative_infected, 0.95, na.rm = TRUE) %>%
  as.numeric()


top_95cum <- cnty_sf %>% filter(cumulative_infected >= p95)



cum_cases_pnts <- cnty_centroids %>%
  left_join(weekly_df %>% filter(mmwr_week == 52),by = "county") %>%
  filter(!is.na(cumulative_infected)) 



##-- color palettes for maps and charts

cat_pal <- 
  c("#31888d", "#52176b", "#f6c143", "#ff66c4", "#61c46e", "#395b8b", "#c1434e")


cat_pal_sm <- 
  c("#31888d", "#52176b", "#f6c143", "#ff66c4" )



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