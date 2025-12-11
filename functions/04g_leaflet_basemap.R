
leaflet_ca_blank <- function(
    initial_zoom   = 6,
    initial_center = c(37.25, -119.5),  # lat, lng
    ...
) {
  leaflet(
    options = leafletOptions(
      #zoomControl      = FALSE,
      #dragging         = FALSE,
      #scrollWheelZoom  = FALSE,
      #doubleClickZoom  = FALSE,
      #touchZoom        = FALSE,
      #boxZoom          = FALSE,
      #keyboard         = FALSE,
      minZoom          = initial_zoom
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




make_cnty_basemap <- function(sf_all) {
  leaflet_ca_blank() %>%
    addPolygons(
      data        = sf_all,
      fillColor   = "#f1f0ea",
      color       = "white",
      weight      = 1,
      fillOpacity = 1
    ) 
}







make_cnty_rank_map <- function(cnty_sf, cnty_pnts) {
  
  leaflet_ca_blank() %>%
    
    addPolygons(
      data        = cnty_sf,
      fillColor   = "#f1f0ea",
      color       = "#cdcabd",
      weight      = 0.8,
      fillOpacity = 1,
      
      label = ~lapply(hover_lbl, htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "bold"),
        textsize  = "14px",
        direction = "auto",
        offset    = c(0, 0),
        opacity   = 1
      )
      
    ) %>%
    
    addCircleMarkers(
      data        = cnty_pnts,
      radius      = ~radius,         
      stroke      = TRUE,
      weight      = 1,
      color       = drk_clr,
      fillColor   = ~pal_priority(priority_tier),
      fillOpacity = 0.9,
    ) 
  
}