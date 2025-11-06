

calc_total_rates_pop <- function(df, ..., pop_col) {
  df %>%
    group_by(...) %>%
    summarise(
      total_pop = first({{ pop_col }}),
      total_infected = max(cumulative_infected, na.rm = TRUE),
      total_inf_prop = total_infected / total_pop,
      inf_rate_100k = round(total_inf_prop * 1e5, 1),
      total_severe = max(cumulative_severe, na.rm = TRUE),
      total_sev_prop = total_severe / total_pop,
      sev_rate_100k = round(total_sev_prop * 1e5, 1),
      total_unrec = max(cumulative_unrecovered, na.rm = TRUE),
      total_unrec_prop = total_unrec / total_pop,
      unrec_rate_100k = round(total_unrec_prop * 1e5, 1),
      .groups = "drop"
    )
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
    arrange({{ mmwr_year }}, {{ mmwr_week }}, ...)
}



