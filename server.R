server <- function(input, output) {
  

#-- reactives to store selections 
  
  selected_county <- reactiveVal(NULL)

  
##--reactive data based on geography selection

filt_dem_df <- reactive({
  req(dem_df, input$dem_selector)
    
    # capture current map selections so this reactive depends on them
    sc <- selected_county()
    
    df <- dem_df %>%
      dplyr::filter(
        geo_level == "county",
        group_var == input$dem_selector
      )
    
  # apply map-based filters if a selection exists
    if (!is.null(sc)) {
      df <- df %>% dplyr::filter(county == sc)
    }
    
    df
}) 




filt_cnty_df <- reactive({
  req(cnty_rates_df)
  
  # capture current map selections so this reactive depends on them
  sc <- selected_county()
  
  df <- cnty_rates_df %>%
    dplyr::filter(
      county == sc
    )
  
  df
  
}) 


cnty_txt_r <- reactive({
  sc <- selected_county()
  
  # If nothing selected → return default message
  if (is.null(sc)) {
    return(
      "<div style='font-size:1.1rem; color:#666;'>
         Select a county on the map for a detailed summary.
       </div>"
    )
  }
  
  df <- filt_cnty_df()
  if (nrow(df) == 0) {
    return(
      "<div style='font-size:1.1rem; color:#666;'>
         Select a county on the map for a detailed summary.
       </div>"
    )
  }
  
  row <- df[1, ]
  
  county_name  <- row$county
  cnty_pop_fmt <- scales::comma(row$group_pop)
  case_fmt <- scales::comma(row$cumulative_infected)
  case_rt_fmt <- scales::comma(round(row$inf_rate_100k, 1))
  sev_fmt <- scales::comma(row$cumulative_severe)
  sev_rt_fmt <- scales::comma(round(row$sev_rate_100k, 1))
  prop_fmt <- scales::percent(row$pop_prop / 100, accuracy = 0.01) 
  
  paste0(
    "<div class='cnty-header' style='font-size: 1.1rem;'>",
    
    "<strong style='font-size: 1.4rem;'>", county_name, "</strong>",
    "<br>",
    
    "<strong>County population</strong>: ", cnty_pop_fmt,
    "<br>",
    "<span style='padding-left: 16px; display: inline-block;'>
     % of CA: <strong>", prop_fmt, "</strong></span>",
    "<br>",
    
    "<strong>Total Infections: ", case_fmt, "</strong>",
    "<br>",
    "<span style='padding-left: 12px; display: inline-block;'>
     Infection rate: <strong>", case_rt_fmt, "</strong> per 100K</span>",
    "<br>",
    
    "<strong>Total Severe Infections: ", sev_fmt, "</strong>",
    "<br>",
    "<span style='padding-left: 12px; display: inline-block;'>
     Severe Infection rate: <strong>", sev_rt_fmt, "</strong> per 100K</span>",
    "<br>",
    
    "</div>"
    
  )
  
  
})

  


output$cnty_txt <- renderUI({
  req(cnty_txt_r())
  HTML(cnty_txt_r())
})

##--reactive table output
output$dem_table <- DT::renderDT({
  req(selected_county())   
  req(filt_dem_df())
  
  df <- filt_dem_df() %>%
    dplyr::select(-c(
      county,
      group_var,
      group_pop,
      health_officer_region,
      total_group_var_pop,
      total_ca_pop,
      geo_level,
      cumulative_severe,
      cumulative_infected
    )) %>%

    rename(
      "Demographic"          = "group_var_cat",
      'Infection Rate'       = "inf_rate_100k",
      'Severe Infection Rate' = "sev_rate_100k"
    )
  
  df %>%
    DT::datatable(
      rownames = FALSE,          
      options = list(
        paging      = FALSE,     
        searching   = FALSE,     
        info        = FALSE,     
        ordering    = TRUE,
        columnDefs  = list(
          list(className = 'dt-left', targets = "_all")
        ),
        dom = 't'
      ),
      class = "compact stripe hover"
    )
  
})




##--maps

# map outputs
output$cnty_map <- renderLeaflet({
  make_cnty_basemap(cnty_sf, hor_sf, cnty_pnts)
})





# observers

# layerId = ~county
observeEvent(input$cnty_map_shape_click, {
  click <- input$cnty_map_shape_click
  req(click$id)  
  
  selected_county(click$id)
  
  # optional: visually highlight selected county
  leafletProxy("cnty_map") %>%
    clearGroup("selected") %>%
    addPolygons(
      data        = cnty_sf %>% dplyr::filter(county == click$id),
      fillColor   = lgt_clr,
      fillOpacity = 0.2, 
      color       = drk_clr,
      weight      = 2,
      group       = "selected"
    )
})




observeEvent(input$reset_cnty, {
  selected_county(NULL)
  
  leafletProxy("cnty_map") %>%
    clearGroup("selected")
})





## plotly scatter that highlights map selection
output$cnty_scatter <- plotly::renderPlotly({
  req(cnty_rates_df)
  sc  <- selected_county()
  
  p_df <- cnty_rates_df %>%
    dplyr::filter(county != "Los Angeles") %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b style='font-size:14px;'>", county, "</b><br>",
        "Proportion of CA: <b>", round(pop_prop, 2), "%</b><br>",
        "Severe Infection Rate: <b>", round(sev_rate_100k, 1), "</b>"
      )
    )
  
  plt <- plotly::plot_ly(
    data       = p_df,
    x          = ~pop_prop,
    y          = ~sev_rate_100k,
    type       = "scatter",
    mode       = "markers",
    text       = ~hover_text,
    hoverinfo  = "text",
    marker     = list(
      size    = 12,
      color   = "#fdacb8",
      line    = list(color = "#892a68", width = 0.5),
      opacity = 0.8
    ),
    source = "cnty_scatter"
  )
  
  if (!is.null(sc)) {
    plt <- plt %>%
      plotly::add_markers(
        data = p_df %>% dplyr::filter(county == sc),
        x    = ~pop_prop,
        y    = ~sev_rate_100k,
        marker = list(
          size  = 15,
          color = "rgba(0,0,0,0)",
          line  = list(color = "#fbd113", width = 5)
        ),
        hoverinfo  = "skip",
        showlegend = FALSE
      )
  }
  
  plt <- plt %>%
    plotly::layout(
      margin = m,
      title = list(text = ""),
      
      xaxis = list(
        title = list(text = "Percent of California Population (%)", standoff = 12)
      ),
      
      yaxis = list(
        automargin = TRUE,
        title = list(text = "Severe Infection Rate (per 100K)", standoff = 12)
      )
      
      
    ) %>%
    
    plotly_drk_theme() 
  
  plt
  
})












##############
# End Server #
############## 
}
