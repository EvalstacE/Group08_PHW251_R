
###- '_common04.R' runs first (brings in all functions)
#### -- then sources this file 


##-- Bring in shapefiles

cnty_week_pnts <- read.csv(file = here("data/weekly_case_points/cnty_week_pnts.csv"))
hor_week_pnts <- read.csv(file = here("data/weekly_case_points/hor_week_pnts.csv"))


geoms <- bring_in_sfs()
ca_state_sf <- geoms$ca_state_sf

ca_cnty_sf <- geoms$ca_cnty_sf %>%
  rename("health_officer_region" = "hlth_f_") %>%
  left_join(inf_rates_by_cnty, by = c("county", "health_officer_region")) %>%
  add_EQ_labels(rename_before = FALSE)

ca_cnty_pnts <- read.csv(file = here("data/weekly_case_points/ca_cnty_pnts.csv")) %>%
  left_join(inf_rates_by_cnty, by = c("county")) %>%
  add_EQ_labels(rename_before = FALSE)


hor_sf <- geoms$hor_sf %>%
  rename("health_officer_region" = "hlth_f_") %>%
  left_join(inf_rates_by_HOR, by = "health_officer_region") 

hor_pnts <- read.csv(file = here("data/weekly_case_points/hor_pnts.csv")) %>%
  left_join(inf_rates_by_HOR, by = c("health_officer_region")) 



##-- create new objects for mapping

sev_scale <- 
  max(cnty_week_pnts$inf_rate_100k, na.rm = TRUE) /
  max(cnty_week_pnts$sev_rate_100k, na.rm = TRUE)



break_cols <- c("case_breaks", "inf_rate_breaks",
                "sev_case_breaks", "sev_rate_breaks")


apply_break_levels <- function(df, break_cols) {
  df %>%
    mutate(across(all_of(break_cols), enforce_factor_levels))
}

cnty_week_pnts <- apply_break_levels(cnty_week_pnts, break_cols)
ca_cnty_pnts   <- apply_break_levels(ca_cnty_pnts,   break_cols)
ca_cnty_sf     <- apply_break_levels(ca_cnty_sf,     break_cols)




top_cnty_rates <- ca_cnty_sf %>%
  filter(
    inf_rate_100k > 4648 |
    sev_rate_100k > 195
  ) %>%
  distinct()



ca_cnty_pnts <- ca_cnty_pnts %>% 
  create_alpha_vals(
    breaks = c(
      inf_rate_breaks = "alpha_val_inf",
      sev_rate_breaks = "alpha_val_sev",
      case_breaks     = "alpha_val_case"
    )
  )




##-- color palettes for maps
custom_pal <- c(
  "#fdacb8",
  "#b93f76",
  "#892a68",
  "#52176b",
  "#1e0c47"
)



