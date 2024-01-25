# app.R
# Check and install required packages
required_packages <- c("jsonlite", "shiny")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

library(jsonlite)
library(shiny)
source("computeRI.R")

ui <- fluidPage(
  fluidPage(
    br(),
    img(src = "logoIRSJD.jpg", width = "15%", height = "auto", align = "right"),
    img(src = "logoCREB.png", width = "15%", height = "auto", align = "left"),
    column(width = 12, align = "center", titlePanel(strong("CSF Reference Intervals Calculator")),  br(), br()),
    sidebarLayout(
      sidebarPanel(
        strong("Introduce measured CSF concentrations and patient's age to get the reference values: "),
        br(),
        br(),
        textInput("value", "CSF concentration (nmol/L):"),
        fluidRow(
          column(3, textInput("age", "Age:")),
          column(3, selectInput("age_unit", "Age unit: ", choices = c("Months", "Years")))
        ),
        selectInput("metab", "Choose metabolite on study:", choices = c("HVA", "5-HIAA")),
        actionButton("compute", strong("Get RI"), align = "center"),
        width = 6
      ),
      mainPanel(
        strong("Computed reference intervals are:"),
        textOutput("classification"),
        br(),
        strong("Reference values at given age are:"),
        textOutput("limits"),
        plotOutput("resultPlot"),
        width = 6
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$compute, {
    metab <- input$metab
    value <- input$value
    age <- input$age
    age_unit <- input$age_unit
    
    result <- computeRI(metab, value, age, age_unit)
    class <- result[[1]]
    limits<- paste(result[[2]], '-', result[[3]])
    
    output$classification <- renderText(class)
    output$limits <- renderText(limits)
    
    output$resultPlot <- renderPlot(plotPointWithinRI(metab, value, age, age_unit))
   })

}


shinyApp(ui, server)
