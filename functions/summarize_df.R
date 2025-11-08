summarize_df <- function(df, examples_n = 5) {
  df %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        .names = "{.col}__{.fn}",
        .fns = list(
          class     = ~ class(.x)[1],
          n_unique  = ~ {
            if (is.numeric(.x)) NA_integer_ else dplyr::n_distinct(.x, na.rm = TRUE)
          },
          examples  = ~ {
            if (is.numeric(.x)) {
              r <- range(.x, na.rm = TRUE)
              if (all(is.infinite(r))) NA_character_ else paste0(r[1], " - ", r[2])
            } else if (inherits(.x, "Date")) {
              u <- unique(.x); u <- u[!is.na(u)]
              if (length(u) == 0) NA_character_
              else paste0("format: ", as.character(head(u, 1)))
            } else if (is.character(.x) || is.factor(.x) || is.logical(.x)) {
              u <- unique(.x); u <- u[!is.na(u)]
              if (length(u) == 0) NA_character_
              else paste(head(as.character(u), examples_n), collapse = ", ")
            } else {
              NA_character_
            }
          }
        )
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to  = c("variable", ".value"),
      names_pattern = "^(.*)__(class|n_missing|n_unique|examples)$"
    ) %>%
    dplyr::arrange(class, variable)
}
