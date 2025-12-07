
ui <- fluidPage(
  
theme = bs_theme(
    version = 5, 
    bootswatch = "cosmo"
),



layout_column_wrap(
  width = NULL, height = 800,
  style = css(grid_template_columns = "1.5fr 1fr"),
  
    card(
      card_header(
        div(
          style = paste0(
            "text-align: center;",
            "padding: 1rem;"
          ),
          
          div(
            "Severe Infection Rates by County, California",
            style = paste0(
              "font-weight: 700;",
              "font-size: 1.25rem;",
              "color: ", drkst_clr, ";"
            )
          ),
          
          div(
            "(Rates per 100K population)",
            style = paste0(
              "font-size: 0.85rem;",
              "font-weight: 400;",
              "color: ", drkst_clr, ";",
              "opacity: 0.8;",
              "margin-top: 0.25rem;"
            )
          )
        )
      ),
      
      leafletOutput("cnty_map")
    ),
    
  layout_column_wrap(
      width = 1,
      heights_equal = "row",
      style = "margin-top: 5rem",
      uiOutput("cnty_txt"),
      
      selectInput(
        inputId = "dem_selector",
        label   = "Select Demographic",
        choices = c(
          "Age Category"   = "age_cat",
          "Race/Ethnicity" = "race_short",
          "Sex"            = "sex"
        ),
        multiple = FALSE
      ),
      
      DT::DTOutput("dem_table")
                       
    )      


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
  height = 700,
  style = paste0("background-color:", drkst_clr, ";"),
  
  card_header(
    div(
      style = paste0(
        "text-align: center;",
        "padding: 1rem;"
      ),

      div(
        "County Proportion of the State's Population and Severe Infection Rate",
        style = paste0(
          "font-weight: 700;",
          "font-size: 1.25rem;",
          "color: ", lgt_clr, ";"
        )
      ),
      
      div(
        "Rates per 100K · Los Angeles County excluded",
        style = paste0(
          "font-size: 0.85rem;",
          "font-weight: 400;",
          "color: ", lgt_clr, ";",
          "opacity: 0.8;",
          "margin-top: 0.25rem;"
        )
      )
    )
  ),
  
  card_body(
    plotlyOutput("cnty_scatter")
  )
)

      

    

    
    




##############
#   End UI   #
##############
)