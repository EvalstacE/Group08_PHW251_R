
calc_rates_overall_hor_cnty <- function(df,
                                        week           = 52,
                                        region_col     = health_officer_region,
                                        county_col     = county,
                                        region_pop_col = total_HOR_pop,
                                        county_pop_col = total_cnty_pop,
                                        ca_pop_col     = total_ca_pop,
                                        infected_col   = cumulative_infected,
                                        severe_col     = cumulative_severe) {
  

  region_col     <- rlang::enquo(region_col)
  county_col     <- rlang::enquo(county_col)
  region_pop_col <- rlang::enquo(region_pop_col)
  county_pop_col <- rlang::enquo(county_pop_col)
  ca_pop_col     <- rlang::enquo(ca_pop_col)
  infected_col   <- rlang::enquo(infected_col)
  severe_col     <- rlang::enquo(severe_col)
  
# filter to last week
  df_week <- df %>%
    dplyr::filter(mmwr_week == week)
  
#-- health officer region-level
  region_rates <- df_week %>%
    dplyr::group_by(!!region_col) %>%
    dplyr::summarise(
      cumulative_infected = sum(!!infected_col, na.rm = TRUE),
      cumulative_severe   = sum(!!severe_col,   na.rm = TRUE),
      group_pop           = first(!!region_pop_col),
      total_ca_pop        = first(!!ca_pop_col),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      inf_rate_100k = dplyr::if_else(group_pop > 0,
                                     (cumulative_infected / group_pop) * 1e5,
                                     NA_real_),
      sev_rate_100k = dplyr::if_else(group_pop > 0,
                                     (cumulative_severe / group_pop) * 1e5,
                                     NA_real_),
      group_var_cat = "Overall",
      group_var     = "All",
      geo_level     = "region",
      total_group_var_pop = sum(group_pop, na.rm = TRUE)
    )
  
  
#--county-level
  county_rates <- df_week %>%
    dplyr::group_by(!!region_col, !!county_col) %>%
    dplyr::summarise(
      cumulative_infected = sum(!!infected_col,   na.rm = TRUE),
      cumulative_severe   = sum(!!severe_col,     na.rm = TRUE),
      group_pop           = first(!!county_pop_col),
      total_ca_pop        = first(!!ca_pop_col),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      inf_rate_100k = dplyr::if_else(group_pop > 0,
                                     (cumulative_infected / group_pop) * 1e5,
                                     NA_real_),
      sev_rate_100k = dplyr::if_else(group_pop > 0,
                                     (cumulative_severe / group_pop) * 1e5,
                                     NA_real_),
      group_var_cat = "Overall",
      group_var     = "All",
      geo_level     = "county",
      total_group_var_pop = sum(group_pop, na.rm = TRUE)
    )
  
  

#--statewide
statewide_rate <- df_week %>%
    dplyr::summarise(
      cumulative_infected = sum(!!infected_col,   na.rm = TRUE),
      cumulative_severe   = sum(!!severe_col,     na.rm = TRUE),
      group_pop           = first(!!ca_pop_col),
      total_ca_pop        = first(!!ca_pop_col),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      inf_rate_100k = dplyr::if_else(group_pop > 0,
                                     (cumulative_infected / group_pop) * 1e5,
                                     NA_real_),
      sev_rate_100k = dplyr::if_else(group_pop > 0,
                                     (cumulative_severe / group_pop) * 1e5,
                                     NA_real_),
      group_var_cat = "Overall",
      group_var     = "All",
      geo_level     = "statewide",
      health_officer_region = "statewide",
      county = "statewide",
      total_group_var_pop = sum(group_pop, na.rm = TRUE)
    )
  


################################
## bind all dfs
  
  out <- dplyr::bind_rows(region_rates, county_rates, statewide_rate) %>%
    dplyr::select(
      !!region_col,
      !!county_col,
      group_var_cat,
      group_var,
      group_pop,
      cumulative_infected,
      cumulative_severe,
      inf_rate_100k,
      sev_rate_100k,
      total_group_var_pop,
      total_ca_pop,
      geo_level
    )
  
  out
}










counts_week_groups <- function(df, mmwr_year, mmwr_week, ...) {
  df %>%
    group_by({{ mmwr_year }}, {{ mmwr_week }}, ...) %>%
    summarise(
      total_new = sum(new_infections, na.rm = TRUE),
      total_cum_infected = sum(cumulative_infected, na.rm = TRUE),
      total_new_severe = sum(new_severe, na.rm = TRUE),
      total_cum_severe = sum(cumulative_severe, na.rm = TRUE),
      total_new_unrec = sum(new_unrecovered, na.rm = TRUE),
      total_cum_unrec = sum(cumulative_unrecovered, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(..., {{ mmwr_year }}, {{ mmwr_week }})
}



