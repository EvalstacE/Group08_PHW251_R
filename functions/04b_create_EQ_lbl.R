

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
  
  x <- dplyr::pull(data, !!var_quo)
  
  # compute equal interval breaks
  breaks_obj <- classInt::classIntervals(x, n = n, style = "equal")
  brks <- breaks_obj$brks
  
  if (compact) {
    labels <- make_compact_labels(brks, add_plus_to_last = TRUE, round_fn = round_fn)
  } else {

    brks_round <- round_fn(brks)
    formatted  <- format(brks_round, big.mark = ",", scientific = FALSE, trim = TRUE)
    labels <- paste0(
      formatted[-length(formatted)],
      " - ",
      formatted[-1]
    )
  }
  
  data %>%
    mutate(
      !!new_col := cut(
        !!var_quo,
        breaks = brks,
        include.lowest = TRUE,
        labels = labels,
        ordered_result = TRUE 
      )
    )
}



