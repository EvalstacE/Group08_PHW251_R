
ui <- fluidPage(
  
theme = bs_theme(
    version = 5, 
    bootswatch = "cosmo"
),



layout_column_wrap(
  width = NULL, height = 600,
  style = css(grid_template_columns = "1.5fr 1fr"),
  
    leafletOutput("cnty_map"),
    
    card(
      
      uiOutput("cnty_txt"),
      
      selectInput(
        inputId = "dem_selector", 
        label   = "Select Demographic",
        choices = unique(dem_df$group_var),
        multiple = FALSE
      ),
      
      DT::DTOutput("dem_table")
      
      
    )
  
  
),

actionButton("reset_cnty", "Clear Selection"),  


layout_column_wrap(width = 1,
      card(
        card_header(
          "County Proportion of the State's Population and Severe Infection Rate"
          ),
          card_body(
            plotlyOutput("cnty_scatter")

        )
      )
      
)
    

    
    




##############
#   End UI   #
##############
)