ui = fluidPage(
  # Title
  titlePanel("Lower Columbia Salmon Indicator Dashboard"),
  #logo
  fluidRow(
    column(12, align = "left",
           img(src = "https://privatelands.wdfw.wa.gov/wdfwlogo_clrnotxt.png", height = "160px")
    )
  ),
  # Shiny UI components
  selectInput("Species", "Select Species", choices = c("None", unique(indicators$Species)), multiple = FALSE),
  selectInput("Run", "Select Run", choices = c("None", unique(indicators$Run)), multiple = FALSE),
  selectInput("Population", "Select Population", choices = c("None", unique(indicators$Population)), multiple = FALSE),
  downloadButton("downloadData", "Download Data"),
  
  tags$style("
    #downloadButton {
      clear: both;
    }
    #plot {
      float: left;
    }
    #range_map {
      float: right;
    }
  "),
  #placeholder for the plot
  plotOutput("plot", width = "66%", height = "600px"),  
  # This creates a placeholder for the ,map
  plotOutput("range_map", width = "33%", height = "600px")
)
