server <- function(input, output) {
  

#-- reactives to store selections 
  
  selected_county <- reactiveVal(NULL)

  
##--reactive data based on geography selection

filt_dem_df <- reactive({
  req(dem_df, input$dem_selector)
    
  # map selection
  sc <- selected_county()
  
  df <- dem_df %>%
    dplyr::filter(group_var == input$dem_selector)
    
  # apply map-based filters if a selection exists
    if (!is.null(sc)) {
      df <- df %>% dplyr::filter(county == sc)
    }
    
    df
}) 




filt_cnty_df <- reactive({
  req(cnty_ranked_df)
  
  # capture current map selections so this reactive depends on them
  sc <- selected_county()
  
  df <- cnty_ranked_df %>%
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
  cnty_pop_fmt <- scales::comma(row$total_cnty_pop)
  case_fmt <- scales::comma(row$cumulative_infected)
  case_rt_fmt <- scales::comma(round(row$adj_rate_100k, 1))
  sev_fmt <- scales::comma(row$cumulative_severe)
  sev_rt_fmt <- scales::comma(round(row$adj_sev_100k, 1))
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
     Infection AAR: <strong>", case_rt_fmt, "</strong> per 100K</span>",
    "<br>",
    
    "<strong>Total Severe Infections: ", sev_fmt, "</strong>",
    "<br>",
    "<span style='padding-left: 12px; display: inline-block;'>
     Severe Infection AAR: <strong>", sev_rt_fmt, "</strong> per 100K</span>",
    "<br>",
    
    "</div>"
    
  )
  
  
})

  


output$cnty_txt <- renderUI({
  req(cnty_txt_r())
  HTML(cnty_txt_r())
})



#########################
##--reactive table output

output$dem_table <- DT::renderDT({
  req(selected_county())   
  
  df <- req(filt_dem_df())
  
  dem_display_label <- names(dem_choices)[dem_choices == input$dem_selector]
  
  
  # highlight highest rate per demographic group  
  highlight_rows <- which(df$high_grp == "yes")
  
  df <- df %>%
    dplyr::select(-c(county, group_var, hgst_rt)) %>%
    mutate(
      inf_rate_100k = scales::comma(inf_rate_100k, accuracy = 1),
      sev_rate_100k = scales::comma(sev_rate_100k, accuracy = 1)
    ) %>%
    rename(
      !!dem_display_label := group_var_cat,
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
          list(className = 'dt-center', targets = "_all"),
          list(visible = FALSE, targets = 3)
        ),
        dom = 't'
      ),
      class = "compact hover"
    ) %>%
    
    formatStyle(
      "high_grp",
      target = "row",
      fontWeight = styleEqual(
        c("yes", "no"),
        c("bold", "normal")
      )
    )
  
})




##--maps

# map outputs
output$cnty_map <- renderLeaflet({
  make_cnty_basemap(cnty_sf, cnty_dorl_centroids)
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



### ranked bar plot --
output$cnty_rank_bar <- plotly::renderPlotly({
  req(cnty_ranked_plot_df)
  sc  <- selected_county()

plt <- plotly::plot_ly(
    data       = cnty_ranked_plot_df,
    x          = ~adj_sev_100k,
    y          = ~county,           
    type       = "bar",
    orientation = "h",
    color      = ~priority_tier,
    colors     = rnk_pal,
    text        = ~hover_lbl,   
    hovertemplate = "%{text}<extra></extra>"      

  ) %>%
  
  layout(
    showlegend = FALSE,
    xaxis = list(
      title    = "Age-Adjusted Severe Infection Rate"
    ), 
    yaxis = list(
      title    = "",
      tickmode = "array",
      tickvals = levels(cnty_ranked_plot_df$county),
      ticktext = tick_labels$county_label_html,
      autorange = "reversed"
    )
  )

})



output$priority_legend <- renderUI({
  map_legend_ui(
    pal_fun          = pal_priority,
    title            = "",
    items_per_column = 1,
    label_base_px    = 16,
    break_factor     = factor(
      c("Top Priority", "Second Priority", "Third Priority"),
      levels = c("Top Priority", "Second Priority", "Third Priority")
    )
  )
})











##############
# End Server #
############## 
}
