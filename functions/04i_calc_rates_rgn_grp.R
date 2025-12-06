

calc_rates_rng_grp  <- function(df,
                                group_var,      
                                pop_col,        
                                week        = 52,
                                region_col  = health_officer_region,
                                county_col  = county,
                                infected_col = cumulative_infected,
                                severe_col   = cumulative_severe,
                                geo_level    = c("region", "county", "statewide")) {
  
  group_var    <- rlang::enquo(group_var)
  pop_col      <- rlang::enquo(pop_col)
  region_col   <- rlang::enquo(region_col)
  county_col   <- rlang::enquo(county_col)
  infected_col <- rlang::enquo(infected_col)
  severe_col   <- rlang::enquo(severe_col)
  geo_level    <- match.arg(geo_level)
  
  group_var_name <- rlang::as_name(group_var)
  
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
      dplyr::group_by(!!region_col, !!county_col, !!group_var) %>%
      dplyr::summarise(
        group_pop           = dplyr::first(!!pop_col),
        cumulative_infected = sum(!!infected_col, na.rm = TRUE),
        cumulative_severe   = sum(!!severe_col,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::group_by(!!region_col, !!group_var) %>%
      dplyr::summarise(
        group_pop           = sum(group_pop,           na.rm = TRUE),
        cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
        cumulative_severe   = sum(cumulative_severe,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::group_by(!!region_col) %>%
      tidyr::complete(
        !!group_var := all_groups,
        fill = list(
          group_pop           = 0,
          cumulative_infected = 0,
          cumulative_severe   = 0
        )
      ) %>% ungroup() %>%
      mutate(county = NA_character_) %>%
      relocate(county, .after = health_officer_region)
      
    
  } else if (geo_level == "county")  {
    
#--county-level
    df_week %>%
      dplyr::group_by(!!region_col, !!county_col, !!group_var) %>%
      dplyr::summarise(
        group_pop           = sum(!!pop_col,      na.rm = TRUE),
        cumulative_infected = sum(!!infected_col, na.rm = TRUE),
        cumulative_severe   = sum(!!severe_col,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::group_by(!!region_col, !!county_col) %>%
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
    
#--statewide (no county or hor grouping) 
    
    df_week %>%
  ##-- first collapse to one row per county and group
      dplyr::group_by(!!county_col, !!group_var) %>%
      dplyr::summarise(
        group_pop_county    = dplyr::first(!!pop_col),
        cumulative_infected = sum(!!infected_col, na.rm = TRUE),
        cumulative_severe   = sum(!!severe_col,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
  ##-- then sum across counties to get statewide totals by group
      dplyr::group_by(!!group_var) %>%
      dplyr::summarise(
        group_pop           = sum(group_pop_county,    na.rm = TRUE),
        cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
        cumulative_severe   = sum(cumulative_severe,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::complete(
        !!group_var := all_groups,
        fill = list(
          group_pop           = 0,
          cumulative_infected = 0,
          cumulative_severe   = 0
        )
      ) %>%
  ##-- adds "statewide" for joining compatibility
      dplyr::mutate(
        !!region_col := "statewide",
        !!county_col := "statewide"
      )
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
    ) %>%
    dplyr::rename(group_var_cat = !!group_var) %>%
    dplyr::mutate(group_var = group_var_name) %>%
    relocate(group_var, .after = group_var_cat) %>%
    
##--These last two columns added are just a QC checkpoint
###-- if grouping worked properly, population sums 
###---should all equal total California population (39109070)
    
    group_by(group_var) %>%
      mutate(
        total_group_var_pop = sum(group_pop),
               total_ca_pop = 39109070
        ) %>%
    ungroup()
}
