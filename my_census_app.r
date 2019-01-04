library(shiny)
library(maps)
library(mapproj)

source("helpers.R")
counties <- readRDS("data/counties.rds")

# Define UI ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with information
               from the 2010 US Census"),
  
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = list("Percent White" = 1, 
                                 "Percent Black" = 2,
                                 "Percent Hispanic" = 3,
                                 "Percent Asian" = 4), 
                  selected = 1),
      
      sliderInput("range", 
                  label = "Range of Interest:",
                  min = 0, max = 100, value = c(0, 100))
      ),
    mainPanel(
      plotOutput("map")
    )
  )
)

# Define server logic ----
server <- function(input, output){
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Percent white" = counties$white,
                   "Percent black" = counties$black,
                   "Percent hispanic" = counties$hispanic,
                   "Percent asian" = counties$asian)
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var, 
                "Percent White" = "% White",
                "Percent Black" = "% Black",
                "Percent Hispanic" = "% Hispanic",
                "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
}


# Run the app ----
shinyApp(ui, server)