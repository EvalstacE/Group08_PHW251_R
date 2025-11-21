server <- function(input, output) {
  
  
  

ca_cnty_sf_week <- st_read(here("data/shapefiles/ca_cnty_sf_week"), quiet = TRUE)
inf_cnty_time_data <- ca_cnty_sf_week

##--reactive data based on slider input
filteredData <- reactive({
  req(input$mmwr_slider)
  inf_cnty_time_data %>%
  filter(mmwr_wk == input$mmwr_slider)
})  
  
##--convert mmwr weeks to labels
output$mmwr_wk_label <- renderText({
  req(input$mmwr_slider)
  week_info <- inf_cnty_time_data %>%
  filter(mmwr_wk == input$mmwr_slider) %>%
  distinct(mmwr_wk, end_date) %>%
  arrange(end_date)
  
  if (nrow(week_info) == 0) return("")
  paste0("Week Ending: ", format(week_info$end_date[1], "%B %d, %Y"))
  
})
  
##--basemap
output$time_map <- renderLeaflet({
sf_data <- inf_cnty_time_data

leaflet(sf_data) %>%
addPolygons(
layerId = ~county,
fillColor = "#f1f0ea",
color = "#b2b1ac",
weight = 1,
fillOpacity = 1
)
})


##--updated map interacting with slider

observe({
    data <- filteredData() %>%
    filter(cumulative_infected > 0)
    
    leafletProxy("time_map", data = data) %>%
    clearMarkers() %>%
    addCircleMarkers(
    radius = ~log1p(cumulative_infected) * 5,
    fillColor = "#0e2b44",
    fillOpacity = 0.9,
    stroke = FALSE,
    label = ~as.character(cumulative_infected),
    labelOptions = labelOptions(
    noHide = TRUE,
    direction = "center",
    textOnly = TRUE,
    style = list(
    "color" = "white",
    "font-size" = "11px",
    "font-weight" = "bold",
    "text-shadow" = "1px 1px #000"
    )
    ),
    popup = ~paste0(NAME, ": ", cumulative_infected, " cumulative infected")
    ) %>%
    clearControls()
})  




  
}
