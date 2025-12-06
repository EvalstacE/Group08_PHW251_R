format_compact_num <- function(x) {
  dplyr::case_when(
    x >= 1e6 ~ paste0(round(x / 1e6), "M"),
    x >= 1e3 ~ paste0(round(x / 1e3), "K"),
    TRUE     ~ as.character(round(x))
  )
}


make_compact_labels <- function(brks, add_plus_to_last = TRUE, round_fn = floor) {
  # round numeric breaks first
  brks_round <- round_fn(brks)
  
  left_vals  <- brks_round[-length(brks_round)]
  right_vals <- brks_round[-1]
  
  left  <- format_compact_num(left_vals)
  right <- format_compact_num(right_vals)
  
  labels <- paste0(left, " - ", right)
  
  if (add_plus_to_last) {
    last_left  <- left[length(left)]
    last_right <- right[length(right)]
    
    labels[length(labels)] <-
      if (last_left == last_right) {
        # e.g. 4K - 4K  -->  "4K+"
        paste0(last_left, "+")
      } else {
        # e.g. 738K - 886K  --> "738K - 886K+"
        paste0(last_left, " - ", last_right, "+")
      }
  }
  
  labels
}


create_EQ_lbl <- function(data, var, n = 6, new_col = NULL,
                          round_fn = floor,
                          compact = TRUE) {
  
  var_quo  <- rlang::enquo(var)
  var_name <- rlang::as_name(var_quo)
  
  if (is.null(new_col)) {
    new_col <- paste0(var_name, "_eq")
  }
  
  # name for the "raw range" bin column
  bins_col <- paste0(new_col, "_bins")
  
  x <- dplyr::pull(data, !!var_quo)
  
  # compute equal interval breaks
  breaks_obj <- classInt::classIntervals(x, n = n, style = "equal")
  brks <- breaks_obj$brks
  
  # ---- full (non-compact) labels ----
  brks_round <- round_fn(brks)
  formatted  <- format(brks_round, big.mark = ",", scientific = FALSE, trim = TRUE)
  full_labels <- paste0(
    formatted[-length(formatted)],
    " - ",
    formatted[-1]
  )
  
  # ---- compact labels (if requested) ----
  if (compact) {
    main_labels <- make_compact_labels(
      brks,
      add_plus_to_last = TRUE,
      round_fn = round_fn
    )
  } else {
    main_labels <- full_labels
  }
  
  data %>%
    mutate(
      # always store non-compact range here
      !!bins_col := cut(
        !!var_quo,
        breaks         = brks,
        include.lowest = TRUE,
        labels         = full_labels,
        ordered_result = TRUE
      ),
      # main labeled factor (compact or full depending on `compact`)
      !!new_col := cut(
        !!var_quo,
        breaks         = brks,
        include.lowest = TRUE,
        labels         = main_labels,
        ordered_result = TRUE
      )
    )
}







add_EQ_labels <- function(
    data,
    rename_before = TRUE,
    vars = c(
      cumulative_infected = "case_breaks",
      inf_rate_100k       = "inf_rate_breaks",
      cumulative_severe   = "sev_case_breaks",
      sev_rate_100k       = "sev_rate_breaks"
    )
) {
  
  if (rename_before) {
    data <- data %>%
      rename(
        cumulative_infected = total_infected,
        cumulative_severe   = total_severe
      )
  }
  
  out <- data
  
  for (i in seq_along(vars)) {
    var_name <- names(vars)[i]
    new_name <- vars[[i]]
    
    # set number of bins:
    n_val <- if (new_name == "case_breaks") 7 else 5
    
    out <- out %>%
      create_EQ_lbl(
        var      = !!rlang::sym(var_name),
        n        = n_val,
        new_col  = new_name,
        round_fn = floor
      )
  }
  
  out
}