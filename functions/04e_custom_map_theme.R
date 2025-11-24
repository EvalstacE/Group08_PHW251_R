
plot_theme <- list(
  theme = theme( 
    plot.margin = margin(t = 1, b = 1, l = 4, r = 4),
    plot.title    = element_text(color = "#0f172a", face = "bold", size = 14),
    plot.subtitle = element_text(color = "#0f172a", size = 10, margin = margin(b = 2)),
    
    plot.caption = ggtext::element_markdown(
      color = "#0f172a",
      size  = 6,
      hjust = 0,
      face  = "italic",
      margin = margin(t = 1, r = 0, b = 0, l = 0)
    ),
    
    legend.position = c(0.2, 0.3),
    legend.justification = "right",
    legend.title.position = "top",
    legend.direction = "vertical",

    legend.title      = element_text(color = "#0f172a", size = 12),
    legend.text       = element_text(color = "#0f172a", size = 10),
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(0.5, "cm"),

  )
)

##-- scale bar
scale <- list(
  annotation_scale = annotation_scale(
    location = "bl",                     
    width_hint = 0.15,
    height   = unit(0.15, "cm"),
    text_cex = 0.6,
    pad_x = unit(1, "cm"),
    pad_y = unit(1.5, "cm"),              
    text_col = "#0f172a",
    line_col = "#f1f0ea",
    bar_cols = c("#0f172a", "grey")
  )
)

##-- caption text
caption_text <- glue(
  "<b>Map created by:</b> Erin Curlee, Ana Terzo, Val Stacey on: <b>{format(Sys.Date(), '%m/%d/%Y')} </b>;<br>",
  "<b>Data Sources:</b> CDC PLACES: BRFSS, US Census Bureau;<br>",
  "<b>Map Projection:</b> World Geodetic System 1984 ensemble (WGS 84);<br>",
  "<b>Map created in R:</b> R Core Team (2024); version: 4.4.2 (2024-10-31 ucrt)"
)



apply_custom_theme <- function(plot, include_caption = FALSE) {
  p <- plot + 
    plot_theme$theme +
    scale$annotation_scale
  if (include_caption) {
    p <- p + labs(caption = caption_text)
  }
  p 
}