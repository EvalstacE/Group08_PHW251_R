### -- function: add MMR Week and Year 
add_mmwr_week_columns <- function(data, date_col = "week_start") {
  date_vector <- data[[date_col]]
  mmwr_info <- MMWRweek::MMWRweek(date_vector)
  data <- data %>%
    mutate(
      mmwr_year = factor(mmwr_info$MMWRyear),
      mmwr_week = factor(mmwr_info$MMWRweek)
    )
  return(data)
}

### -- function: add start and end dates of MMWR week
add_start_end_dates <- 
  function(data, year_col = "mmwr_year", week_col = "mmwr_week") {
    year_vec <- as.integer(as.character(data[[year_col]]))
    week_vec <- as.integer(as.character(data[[week_col]]))
    
    data %>%
      mutate(
        start_date = MMWRweek2Date(MMWRyear = year_vec, MMWRweek = week_vec, MMWRday = 1),
        end_date   = MMWRweek2Date(MMWRyear = year_vec, MMWRweek = week_vec, MMWRday = 7)
      )
  }