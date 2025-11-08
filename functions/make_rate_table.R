# make rate table (as seen in milestone 3)
make_rate_table <- function(df,
                            ...,
                            pop_col = total_pop,
                            rate_pattern = "_rate($|_)",
                            drop_cols = "unrec_rate_100k",
                            sort_by = "sev_rate_100k",
                            top_n = NULL,
                            highlight_col = "sev_rate_100k",
                            highlight_bg = "#5ce1e6") {
  
  df2 <- df %>%
    dplyr::select(..., {{ pop_col }}, dplyr::matches(rate_pattern)) %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    dplyr::arrange(dplyr::desc(.data[[sort_by]]))
  
  if (!is.null(top_n)) {
    df2 <- df2 %>% dplyr::slice_head(n = top_n)
  }
  
  hi_col_idx <- match(highlight_col, names(df2))
  
##-- kable styles
  kbl_out <- df2 %>%
    kableExtra::kbl(escape = FALSE, align = "c") %>%
    kableExtra::row_spec(
      0,
      bold = TRUE,
      background = "#0f172a",
      extra_css = "font-size: 16px!important;color:#ffffff;"
    )
  
  if (!is.na(hi_col_idx)) {
    kbl_out <- kbl_out %>%
      kableExtra::column_spec(
        hi_col_idx,
        bold = TRUE,
        background = highlight_bg
      )
  }
  
  kbl_out %>%
    kableExtra::kable_styling(bootstrap_options = c("bordered", "hover", "striped"))
}
