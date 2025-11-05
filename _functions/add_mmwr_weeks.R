### -- function: add MMR Week and Year 
add_mmwr_week_columns <- function(data, date_col = "week_start") {
  date_vector <- data[[date_col]]
  mmwr_info <- MMWRweek::MMWRweek(date_vector)
  data <- data %>%
    mutate(
      MMWRyear = factor(mmwr_info$MMWRyear),
      MMWRweek = factor(mmwr_info$MMWRweek)
    )
  return(data)
}

### -- function: add start and end dates of MMWR week
add_start_end_dates <- 
  function(data, year_col = "MMWRyear", week_col = "MMWRweek") {
    year_vec <- as.integer(as.character(data[[year_col]]))
    week_vec <- as.integer(as.character(data[[week_col]]))
    
    data %>%
      mutate(
        start_date = MMWRweek2Date(MMWRyear = year_vec, MMWRweek = week_vec, MMWRday = 1),
        end_date   = MMWRweek2Date(MMWRyear = year_vec, MMWRweek = week_vec, MMWRday = 7)
      )
  }