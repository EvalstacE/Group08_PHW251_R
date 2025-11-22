map_legend_ui <- function(
    break_factor,
    pal_fun,
    title             = NULL,
    subtitle          = NULL,
    items_per_column  = 2,      
    size_scale        = 1,      
    circle_base_px    = 20,     
    label_base_px     = 12,     
    title_base_px     = 14,
    subtitle_base_px  = 12
) {
  # ensure factor
  f    <- as.factor(break_factor)
  labs <- levels(f)
  cols <- pal_fun(labs)
  
  n <- length(labs)
  
  # if items_per_column is Inf or NULL, just stack everything in one column
  if (is.infinite(items_per_column) || is.null(items_per_column)) {
    idx_list <- list(seq_len(n))
  } else {
    idx_list <- split(seq_len(n), ceiling(seq_len(n) / items_per_column))
  }
  
  # apply global size scale
  circle_size    <- circle_base_px * size_scale
  label_size     <- paste0(label_base_px * size_scale, "px")
  title_size     <- paste0(title_base_px * size_scale, "px")
  subtitle_size  <- paste0(subtitle_base_px * size_scale, "px")
  
  make_items <- function(idxs) {
    lapply(idxs, function(i) {
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 8px;",
        
        # circular color swatch
        tags$div(
          style = paste0(
            "width:",  circle_size, "px;",
            "height:", circle_size, "px;",
            "background-color:", cols[i], ";",
            "border: 1px solid #555;",
            "border-radius: 50%;",
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
tagList(
  div(
    style = "display: flex; flex-direction: column;",
    
    # Legend title
    if (!is.null(title)) div(
      style = paste0(
        "font-weight: 600;",
        "line-height: 0.85;",
        "font-size:", title_size, ";",
        "margin-bottom: 0px;",
        "max-width: 100px;",     
        "white-space: normal;" 
      ),
      title
    ),
    
    # Legend subtitle
    if (!is.null(title)) div(
      style = paste0(
        "font-weight: 300;",
        "font-size:", subtitle_size, ";",
        "margin-bottom: 8px;",
        "max-width: 100px;",     
        "white-space: normal;" 
      ),
      subtitle
    ),
    
    # Row of columns
    div(
      style = "display: flex; align-items: flex-start;",
      columns
    )
  )
)
}
