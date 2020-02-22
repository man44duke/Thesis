# Load packages ----
library(shiny)
library(maps)
library(mapproj)

# Load data ----

values <- readRDS("county_data_fred.RDS")


# Source helper functions -----
source("Visualizations/helper.R")

# User interface ----
ui <- fluidPage(
    titlePanel("County Level Data"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Choose Variable:"),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = colnames(values[,-c(1)]),
                        selected = "percent_population_black"),
            selectInput("color", 
                        label = "Color",
                        choices = c("darkgreen", "black", "darkorange", "darkviolet"),
                        selected = "darkgreen"),
            
            sliderInput("range", 
                        label = "Range of interest:",
                        min = 0, max = 100, value = c(0, 100))
        ),
        
        mainPanel(plotOutput("map"))
    )
)

# Server logic ----
server <- function(input, output, session) {
    output$map <- renderPlot({
        data <- values[,input$var]
        
        color <- input$color
        
        legend <- input$var
        
        percent_map(data, color, legend, input$range[1], input$range[2])
    })
}

# Run app ----
shinyApp(ui, server)