leaflet_ca_blank <- function(
    initial_zoom   = 6,
    initial_center = c(37.25, -119.5),
    ...
) {
  leaflet::leaflet(
    options = leafletOptions(
      minZoom = initial_zoom
    )
  ) %>%
    setView(
      lng  = initial_center[2],
      lat  = initial_center[1],
      zoom = initial_zoom
    ) %>%
    htmlwidgets::onRender("
      function(el, x) {
        // Set outer widget background
        el.style.background = 'white';

        // Set internal Leaflet container background
        var containers = el.getElementsByClassName('leaflet-container');
        if (containers.length > 0) {
          containers[0].style.background = 'white';
        }
      }
    ")
}



make_cnty_basemap <- function(cnty_sf, hor_sf, cnty_pnts) {

 leaflet_ca_blank(map_id = "cnty_map") %>%
    
  addPolygons(
      data        = hor_sf,
      fillColor   = "#f1f0ea",
      color       = drkst_clr,
      fillOpacity = 0.9,
      weight      = 2
      
  ) %>%
    
  addPolygons(
      data        = cnty_sf,
      fillColor   = "transparent",
      color       = "#cdcabd",
      weight      = 0.8,
      
      label = ~lapply(hover_lbl, htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "bold"),
         textsize  = "14px",
         direction = "auto",
         offset    = c(0, 0),
         opacity   = 1
      ),
      
      layerId = ~county,
      highlightOptions = highlightOptions(
        weight       = 2,
        color        = drk_clr,
        fillOpacity  = 0.3,
        bringToFront = FALSE
      )
      
  ) %>%
    
  addCircleMarkers(
      data        = cnty_dorl_centroids,
      radius      = ~radius,         
      stroke      = TRUE,
      weight      = 1,
      color       = drk_clr,
      fillColor   = ~pal_priority(priority_tier),
      fillOpacity = 0.9,
      
  ) 
  
}

