leaflet_ca_blank <- function(
    initial_zoom   = 6,
    initial_center = c(37.25, -119.5),  # lat, lng
    ...
) {
  leaflet(
    options = leafletOptions(
      zoomControl      = FALSE,
      dragging         = FALSE,
      scrollWheelZoom  = FALSE,
      doubleClickZoom  = FALSE,
      touchZoom        = FALSE,
      boxZoom          = FALSE,
      keyboard         = FALSE,
      minZoom          = initial_zoom,
      maxZoom          = initial_zoom
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




make_cnty_basemap <- function(sf_all, sf_top) {
  leaflet_ca_blank() %>%
    addPolygons(
      data        = sf_all,
      fillColor   = "#f1f0ea",
      color       = "white",
      weight      = 1,
      fillOpacity = 1
    ) %>%
    addPolygons(
      data        = sf_top,
      color       = "#cdcabd",
      fillColor   = "#dddbcf",
      weight      = 1.5,
      fillOpacity = 1
    )
}
