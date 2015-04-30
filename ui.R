#iris_example
#ui.R
library(shiny)
library(ggvis)

shinyUI(fluidPage(
  titlePanel("Exploring Clusters in Iris Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "distance", label = "Distance Metric",
                  choices = c("euclidean", "correlation")),
      numericInput("clusters", "Cluster Count", 3, min = 1, max=9),
      dataTableOutput("table")  
    ),
    mainPanel(
      ggvisOutput("plot2")
    )
  )
))