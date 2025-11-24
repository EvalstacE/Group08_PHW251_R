
make_case_bar_df <- function(data, sort_var) {
  sort_var <- rlang::enquo(sort_var)
  
  data %>%
    mutate(
      county = fct_reorder(county, !!sort_var),
      rate_top = case_when(
        county %in% top_inf ~ "Top Infection Rate",
        county %in% top_sev ~ "Top Severe Infection Rate", 
        TRUE                 ~ "others"),
      rate_top = factor(
        rate_top,
        levels = c("others", "Top Infection Rate", "Top Severe Infection Rate"))
    ) %>%
    
    mutate(
      county_label = case_when(
        rate_top == "Top Infection Rate" ~ 
          glue("<span style='font-weight:bold; font-size:12px; color:#b93f76'>{county}</span>"),
        rate_top == "Top Severe Infection Rate" ~ 
          glue("<span style='font-weight:bold; font-size:12px; color:#6a1c6c'>{county}</span>"),
        TRUE  ~ 
          glue("<span style='font-size:8px; color:#555555'>{county}</span>")
      )
    )
  
}


make_label_map <- function(df) {
  df %>%
    distinct(county, county_label) %>%
    tibble::deframe()
}