library(shiny)
library(markdown)

# Define the overall UI
shinyUI(
  fluidPage(    
    
    # Give the page a title
    titlePanel("PCBs in Fish"),
    
    sidebarLayout(      
      sidebarPanel(
        checkboxInput("plot", h4("Plot fish species & waterbody vs PCBs", style = "color:red;")),
        br(),
        checkboxInput("model", h5("Regression model for predicting PCBs", style = "color:red;"))
         
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot species & waterbody vs PCBs",
                             plotOutput("plots")
                            ),
                    tabPanel("Model for predicting PCBs", 
                             uiOutput("info2"),
                             verbatimTextOutput("fitting"),
                             uiOutput("info3")
                    )
        )
        )
    )
  )
)