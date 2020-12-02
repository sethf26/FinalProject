library(readr)
library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(plyr)
library(readxl)
library(ggplot2)
library(robust)
library(MASS)
library(rstanarm)

#Data
NFL_2016_label <- read_excel("raw_data/NFL_2016_label.xlsx", 
                             skip = 3)
NFL_2016 <- NFL_2016_label%>%
  mutate(year = 2016)

NFL_2017_label <- read_excel("raw_data/NFL_2017_label.xlsx", 
                             skip = 3)
NFL_2017 <- NFL_2017_label%>%
  mutate(year = 2017)

NFL_2018_label <- read_excel("raw_data/NFL_2018_label.xlsx", 
                             skip = 3)
NFL_2018 <- NFL_2018_label%>%
  mutate(year = 2018)


NFL_Full_stat <- join(NFL_2016,NFL_2017,by = "year", type = "full")
NFL_Full_stat <- join(NFL_Full_stat,NFL_2018,by = "year", type = "full")

Combine_2016 <- read_excel("raw_data/Combine_2016.xlsx")
Combine_2016 <- Combine_2016%>%
  mutate(year = 2016)

Combine_2017 <- read_excel("raw_data/Combine_2017.xlsx")
Combine_2017 <- Combine_2017%>%
  mutate(year = 2017)

Combine_2018 <- read_excel("raw_data/Combine_2018.xlsx")
Combine_2018 <- Combine_2018%>%
  mutate(year = 2018)

Combine_Full_stat <- join(Combine_2016,Combine_2017,by = "year", type = "full")
Combine_Full_stat <- join(Combine_Full_stat,Combine_2018,by = "year", type = "full")

mm <- rlm(formula = `Approx Val CarAV` ~ Rnd+Pos+G,
          data = NFL_Full_stat
)


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
  tabPanel("Plot of Round and Career Value",
           fluidPage(plotOutput("plot"))
  ),
  
  tabPanel("Regression Analysis",
           fluidPage(verbatimTextOutput("regress")),
           h3("Explanation of Regression"),
           p("This regression accounts for games played,position, and round drafted and the effect on Average Career Value, as we can see, as games played increases, career value does, and earlier rounds bring higher averages. Defensive ends, corners, fullbacks and many other positions are less likely to have higher average career values while quarterbacks and guards receive favorable coefficients.")
  ),
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
  
  output$regress <- renderPrint({
    summary(mm)
  })
  
  output$plot <- renderPlot({
    ggplot(NFL_Full_stat, mapping = aes(x = Rnd, y = `Approx Val CarAV`, color = Pos))+geom_point()+labs(title = "Average Career Value by Round", x = "Round Drafter", y = "Average Career Value")
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}
shinyApp(ui = ui, server = server)