create_alpha_vals <- function(df, breaks) {
  
  for (i in seq_along(breaks)) {
    
    break_col <- names(breaks)[i]
    alpha_col <- breaks[[i]]
    
    df <- df %>%
      mutate(
        !!alpha_col := case_when(
          (!!sym(break_col)) %in% levels(!!sym(break_col))[1:2] ~ 0.15,
          (!!sym(break_col)) %in% levels(!!sym(break_col))[
            (nlevels(!!sym(break_col)) - 1):nlevels(!!sym(break_col))
          ] ~ 1,
          TRUE ~ 0.35
        )
      )
  }
  
  df
}