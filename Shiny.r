library(readr)
library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)


ui <- navbarPage(
  "Final Project Title",
  tabPanel("Summary Stats",
           fluidPage(
             titlePanel("Dataset Summaries"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "dataset",
                             label = "Choose a dataset:",
                             choices = c("NFL", "Combine"
                             ))),
               
               mainPanel(verbatimTextOutput("summary")))
           )),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("My project is focused on utilizing data from the NCAA, NFL Combine, and NFL Statistics to find a link between college performance/athletic testing and NFL performance across different positions."),
           h3("Data Sourcing"),
           p("My data repository can be found here",a("data repo", href = "https://github.com/sethf26/shiny.git"), "and my sources currently include Profootball reference for NFL data, the NFL combine official repository for combine data, and the NCAA for college data, but I have yet to collect full college data due to inconsistencies in the year players enter the draft. I currently have 2 data sets including full NFL and combine stats for draft classes from 2016-2018, and am having trouble joining them by player name due to a pesky backslash present in my data")))

server <- function(input, output) {
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "NFL" = NFL_Full_stat,
           "Combine" = Combine_Full_stat)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}
shinyApp(ui = ui, server = server)
