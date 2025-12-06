drk_theme <- function(base_color = lgt_clr) {
  theme_minimal() +
    theme(
      text = element_text(color = base_color),   # <–– all text uses this color
      axis.title.x = element_text(margin = margin(t = 6), size = 10),
      axis.title.y = element_text(margin = margin(r = 6), size = 10),
      axis.text.x  = element_text(size = 8, color = base_color),
      axis.text.y  = element_text(size = 8, margin = margin(r = 15, l = 10), color = base_color),
      plot.title   = element_text(margin = margin(t = 10, b = 10)),
      plot.caption = element_text(hjust = 0.5),
      
      # backgrounds
      panel.background = element_rect(fill = drkst_clr, color = NA),
      plot.background  = element_rect(fill = drkst_clr, color = NA),
      panel.grid.major = element_line(color = grid_clr),
      panel.grid.minor = element_line(color = grid_clr),
      
      # plot margin
      plot.margin = margin(t = 8, b = 8, l = 18, r = 18)
    )
}



plotly_drk_theme <- function(plt, base_color = lgt_clr) {
  plt %>%
    plotly::layout(
      # backgrounds (panel + outer)
      paper_bgcolor = drkst_clr,
      plot_bgcolor  = drkst_clr,
      
      # default text color
      font = list(color = base_color),
      
      # x-axis styling
      xaxis = list(
        title    = list(
          font = list(size = 10, color = base_color)
        ),
        tickfont = list(size = 8, color = base_color),
        gridcolor = grid_clr,
        zeroline  = FALSE
      ),
      
      # y-axis styling
      yaxis = list(
        title    = list(
          font = list(size = 10, color = base_color)
        ),
        tickfont = list(size = 8, color = base_color),
        gridcolor = grid_clr,
        zeroline  = FALSE
      ),
      
      # title styling (do not change text, just font/color)
      title = list(
        font = list(color = base_color)
      ),
      
      # legend styling (if you ever have one)
      legend = list(
        font = list(color = base_color)
      ),
      

      # hover label styling to match dark theme
      hoverlabel = list(
        bgcolor = drkst_clr,
        font    = list(color = base_color)
      )
    )
}
