map_legend_ui <- function(break_factor, pal_fun, title = NULL) {
  
  # ensure factor
  f    <- as.factor(break_factor)
  labs <- levels(f)
  cols <- pal_fun(labs)
  
  n <- length(labs)
  
  # split indices into chunks of 2 items (max stacked per column)
  idx_list <- split(seq_len(n), ceiling(seq_len(n) / 2))
  
  # sizes (tweak these to your liking)
  circle_size <- 20        # diameter in px
  label_size  <- "14px"    # label text size
  title_size  <- "16px"    # title text size
  
  make_items <- function(idxs) {
    lapply(idxs, function(i) {
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 8px;",
        
        # circular color swatch
        tags$div(
          style = paste0(
            "width:", circle_size, "px;",
            "height:", circle_size, "px;",
            "background-color:", cols[i], ";",
            "border: 1px solid #555;",
            "border-radius: 50%;",   # ← makes it a circle
            "margin-right: 8px;"
          )
        ),
        
        # label text
        tags$span(
          style = paste0("font-size:", label_size, ";"),
          labs[i]
        )
      )
    })
  }
  
  # build a column per chunk
  columns <- lapply(idx_list, function(idxs) {
    tags$div(
      style = "display: flex; flex-direction: column; margin-right: 20px;",
      make_items(idxs)
    )
  })
  
  # final wrapper
  tags$div(
    style = "display: flex; flex-direction: column;",
    
    # Legend title
    if (!is.null(title)) tags$div(
      style = paste0(
        "font-weight: 600;",
        "font-size:", title_size, ";",
        "margin-bottom: 8px;"
      ),
      title
    ),
    
    # Row of columns
    tags$div(
      style = "display: flex; align-items: flex-start;",
      columns
    )
  )
}
