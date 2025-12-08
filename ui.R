
ui <- fluidPage(
  
theme = bs_theme(
    version = 5, 
    bootswatch = "cosmo"
),



div(
  style = paste0(
    "text-align: center;",
    "padding: 1rem;",
    "margin-top: 2rem;"
  ),
  
  div(
    "Weekly Cumulative Infections",
    style = paste0(
      "font-weight: 700;",
      "font-size: 1.25rem;",
      "color: ", drkst_clr, ";"
    )
  ),
  

  div(
    "May 2023 - December 2023",
    style = paste0(
      "font-size: 0.95rem;",
      "font-weight: 400;",
      "color: ", drkst_clr, ";",
      "opacity: 0.8;",
      "margin-top: 0.25rem;"
    )
  )
  
),

  
     
layout_column_wrap( 
    style = "margin: 0rem !important; margin-top: 0.5rem !important;",
    width = 1,
    heights_equal = "row",
    fillable = TRUE,
    
    uiOutput("week_slider"),
    
    uiOutput("mmwr_wk_label"), 
    
    layout_column_wrap(
      width = NULL, height = 620, fill = FALSE,
      style = css(grid_template_columns = "1.3fr 1fr"),
      
      leafletOutput("cnty_case_map", height = "600px"),
      
      plotlyOutput("cum_cases_plot")
    )
    
)




##############
#   End UI   #
##############
)