leaflet_ca_blank <- function(
    initial_zoom   = 5.4,
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


cnty_pal <- colorNumeric(
  palette = custom_pal,
  domain  = cnty_pnts$sev_rate_100k
)

hor_pal <- colorNumeric(
  palette = custom_pal,
  domain = hor_pnts$sev_rate_100k
  )

make_cnty_basemap <- function(cnty_sf, cnty_pnts) {
  leaflet_ca_blank(map_id = "cnty_map") %>%
    
    addPolygons(
      data        = cnty_sf,
      fillColor   = "#f1f0ea",
      color       = "white",
      fillOpacity = 1,
      weight = 1,
      layerId = ~county,
      highlightOptions = highlightOptions(
        weight      = 2,
        color       = drk_clr,
        fillOpacity = 0.3,
        bringToFront = FALSE
      )
      
    ) %>%
    
    addCircleMarkers(
      data        = cnty_pnts,
      radius      = ~radius,         
      stroke      = TRUE,
      weight      = 1,
      color       = drk_clr,
      fillColor   = ~cnty_pal(sev_rate_100k),
      fillOpacity = 0.9
    )
  
  
}


make_hor_basemap <- function(cnty_sf, hor_sf) {
  leaflet_ca_blank(map_id = "hor_map") %>%   

    addPolygons(
      data        = cnty_sf,
      fillColor   = "#f1f0ea",
      color       = "white",
      fillOpacity = 1,
      weight      = 1
    ) %>%

    addPolygons(
      data        = hor_sf,
      fillColor   = "#dddbcf",
      color       = "#cdcabd",
      fillOpacity = 0.5,
      weight      = 1.5,
      layerId     = ~health_officer_region,   
      highlightOptions = highlightOptions(
        weight      = 2,
        color       = drk_clr,
        fillOpacity = 0.8,
        bringToFront = FALSE
      )
    ) %>%
    
    addCircleMarkers(
      data        = hor_pnts,
      radius      = ~radius,         
      stroke      = TRUE,
      weight      = 1,
      color       = drk_clr,
      fillColor   = ~hor_pal(sev_rate_100k),
      fillOpacity = 0.9
    )
}
