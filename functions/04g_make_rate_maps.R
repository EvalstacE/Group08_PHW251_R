make_rate_map <- function(
    cnty_sf,
    ca_state_sf,
    highest_rates_sf,
    cnty_pnts,
    rate_var,      
    breaks_var,
    alpha_var   = "alpha_val_inf",
    palette,
    title       = "Rate (per 100k)",
    scale_factor = 1,
    pad_x       = 0.25,
    pad_y       = 0.1,
    size_values = NULL,
    use_rescale = TRUE,
    size_range  = c(3, 9),
    size_mult   = 0.12,
    plot_title = NULL,
    plot_subtitle = NULL
) {
  
  rate_var   <- rlang::sym(rate_var)
  breaks_var <- rlang::sym(breaks_var)
  
  # -- size scale factor
  if (is.null(size_values)) {
    size_map <- cnty_pnts %>%
      group_by(!!breaks_var) %>%
      summarise(
        med_val = median((!!rate_var) * scale_factor, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        size_val = dplyr::case_when(
          use_rescale ~ scales::rescale(sqrt(med_val), to = size_range),
          TRUE        ~ sqrt(med_val) * size_mult
        )
      ) %>%
      select(!!breaks_var, size_val) %>%
      tibble::deframe()
  } else {

    size_map <- size_values
  }
  
  ##-- creates outer 'box' of coordinates for sf
  bb <- sf::st_bbox(cnty_sf)
  
  x_range <- bb["xmax"] - bb["xmin"]
  y_range <- bb["ymax"] - bb["ymin"]
  
  x_pad <- as.numeric(x_range * pad_x)
  y_pad <- as.numeric(y_range * pad_y)
  
  x_limits <- c(as.numeric(bb["xmin"] - x_pad),
                as.numeric(bb["xmax"] + x_pad))
  y_limits <- c(as.numeric(bb["ymin"] - y_pad),
                as.numeric(bb["ymax"] + y_pad))  
  
  
  # --- get center points for high rate counties
  highest_rates_centroids <- highest_rates_sf %>%
    dplyr::mutate(geometry = sf::st_centroid(geometry)) %>%
    dplyr::mutate(
      lng = sf::st_coordinates(geometry)[,1],
      lat = sf::st_coordinates(geometry)[,2]
    )
  
  ####--- make the map
  ggplot() +
    geom_sf(data = cnty_sf, fill = "#f1f0ea", color = "white") +
    
    geom_sf(
      data     = ca_state_sf,
      fill     = NA,
      color    = alpha("#1e0c47", 0.3),
      linewidth = 0.3
    ) +
    
    as_reference(
      geom_sf(
        data     = highest_rates_sf,
        color    = "black",
        fill     = NA,
        linewidth = 1
      ),
      id = "high_rates"
    ) +
    with_blur("high_rates", sigma = 6) +
    
    geom_sf(
      data  = highest_rates_sf,
      fill  = "#dddbcf",
      color = "#cdcabd"
    ) +
    
    geom_point(
      data = cnty_pnts,
      aes(
        x     = lng,
        y     = lat,
        fill  = !!breaks_var,
        size  = !!breaks_var,
        alpha = .data[[alpha_var]]
      ),
      shape  = 21,
      stroke = 0.5,
      color  = "#1e0c47"
    ) +
    
    scale_alpha_identity(guide = "none") +
    
    scale_size_manual(
      values = size_map,
      name   = stringr::str_wrap(title, 10),
      guide  = guide_legend(override.aes = list(alpha = 1))
    ) +
    
    scale_fill_manual(
      values = palette,
      name   = stringr::str_wrap(title, 10)
    ) +
    

    # --- label layer
    ggrepel::geom_text_repel(
      data = highest_rates_centroids,
      aes(
        x = lng,
        y = lat,
        label = county
      ),
      size = 3.5,
      max.overlaps = Inf
    ) + 
    
    coord_sf(
      xlim   = x_limits,
      ylim   = y_limits,
      expand = FALSE
    ) +    
    
   labs(
      title    = plot_title,
      subtitle = plot_subtitle
    ) +
  
    theme_void() 
}
