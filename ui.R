
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
    "Age-Adjusted Severe Infection Rates",
    style = paste0(
      "font-weight: 700;",
      "font-size: 1.25rem;",
      "color: ", drkst_clr, ";"
    )
  ),
  
  div(
    "by County and their Priority Group",
    style = paste0(
      "font-weight: 700;",
      "font-size: 1.25rem;",
      "color: ", drkst_clr, ";"
    )
  ),
  
  div(
    "(Age-Adjuted Rates (AAR) per 100K population)",
    style = paste0(
      "font-size: 0.85rem;",
      "font-weight: 400;",
      "color: ", drkst_clr, ";",
      "opacity: 0.8;",
      "margin-top: 0.25rem;"
    )
  ),
  
  div(
      style = paste0(
        "display: flex;",
        "justify-content: center;",
        "align-items: center;",
        "width: 100%;",
        "margin-top: 1rem;"
      ),
      
      div(
        style = paste0(
          "display: inline-flex;",
          "justify-content: center;"
        ),
        uiOutput("priority_legend")
      )
  )
  
),


layout_column_wrap(
  width = NULL, height = 800,
  style = css(grid_template_columns = "1.5fr 1fr"),
  

      leafletOutput("cnty_map"),


       plotlyOutput("cnty_rank_bar")
    
),



div(
  style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
  actionButton(
    "reset_cnty", "Clear Selection", width = 500,
    style = paste0(
      "background-color: ", drkst_clr, ";",
      "color: ", lgt_clr, ";",
      "font-weight: 600;",
      "font-size: 1.1rem;",
      "padding: 12px 28px;",
      "border-radius: 6px;",
      "border: none;"
    )
  )
),


card(
    uiOutput("cnty_txt"),
    
    selectInput(
      inputId = "dem_selector",
      label   = "Select Demographic",
      choices = dem_choices,
      multiple = FALSE
    ),
    
    DT::DTOutput("dem_table")

)
    

    
    




##############
#   End UI   #
##############
)