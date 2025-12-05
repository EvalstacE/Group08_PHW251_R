
calc_rates_rng_grp  <- function(df,
                                group_var,      
                                pop_col,        
                                week        = 52,
                                region_col  = health_officer_region,
                                county_col  = county,
                                infected_col = cumulative_infected,
                                severe_col   = cumulative_severe,
                                geo_level    = c("region", "county")) {
  
  group_var    <- rlang::enquo(group_var)
  pop_col      <- rlang::enquo(pop_col)
  region_col   <- rlang::enquo(region_col)
  county_col   <- rlang::enquo(county_col)
  infected_col <- rlang::enquo(infected_col)
  severe_col   <- rlang::enquo(severe_col)
  geo_level    <- match.arg(geo_level)
  
  #-- get *all* possible group levels (e.g. all 7 race categories)
  all_groups <- df %>%
    dplyr::pull(!!group_var) %>%
    unique()
  
  #-- restrict to last week only (defaults to 52)
  df_week <- df %>%
    dplyr::filter(mmwr_week == week)
  
  #-- geography-specific aggregation
  out <- if (geo_level == "region") {
    
#-- health officer region-level
    df_week %>%
##-- one row per region–county–group
      dplyr::group_by(!!region_col, !!county_col, !!group_var) %>%
      dplyr::summarise(
        group_pop           = dplyr::first(!!pop_col),
        cumulative_infected = sum(!!infected_col, na.rm = TRUE),
        cumulative_severe   = sum(!!severe_col,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
##-- sum across counties within region–group
      dplyr::group_by(!!region_col, !!group_var) %>%
      dplyr::summarise(
        group_pop           = sum(group_pop,           na.rm = TRUE),
        cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
        cumulative_severe   = sum(cumulative_severe,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
##-- make sure every region has every group level
      dplyr::group_by(!!region_col) %>%
      tidyr::complete(
        !!group_var := all_groups,
        fill = list(
          group_pop           = 0,
          cumulative_infected = 0,
          cumulative_severe   = 0
        )
      ) %>%
      dplyr::ungroup()
    
  } else {
    
#--county-level
    df_week %>%
      dplyr::group_by(!!county_col, !!group_var) %>%
      dplyr::summarise(
        group_pop           = sum(!!pop_col,      na.rm = TRUE),
        cumulative_infected = sum(!!infected_col, na.rm = TRUE),
        cumulative_severe   = sum(!!severe_col,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
##--make sure every county has every group level
      dplyr::group_by(!!county_col) %>%
      tidyr::complete(
        !!group_var := all_groups,
        fill = list(
          group_pop           = 0,
          cumulative_infected = 0,
          cumulative_severe   = 0
        )
      ) %>%
      dplyr::ungroup()
  }
  
  
#################  
#-- compute rates
  out %>%
    dplyr::mutate(
      inf_rate_100k = dplyr::if_else(
        group_pop > 0,
        round(1e5 * cumulative_infected / group_pop),
        0
      ),
      sev_rate_100k = dplyr::if_else(
        group_pop > 0,
        round(1e5 * cumulative_severe / group_pop),
        0
      )
    )
}
