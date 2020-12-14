
#Load libraries to be used

library(readr)
library(shiny)
library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(robust)
library(MASS)
library(rstanarm)
library(tidyr)
library(rvest)
library(rlist)
library(XML)
library(httr)
library(taRifx)
library(readr)
library(gt)
library(gtsummary)
library(fec16)
library(mdthemes)
library(broom.mixed)
library(DT)
library(shinythemes)

#Load in full dattacreated in data gathering

full_data <- read_csv("fulldata.csv", col_types = cols(`X1` = col_integer()))

#Load in positional datasets created in data gathering

QB <- read_csv("QB.csv", col_types = cols(`X1` = col_integer()))
RB <- read_csv("RB.csv", col_types = cols(`X1` = col_integer()))
LB <- read_csv("LB.csv", col_types = cols(`X1` = col_integer()))
DT <- read_csv("DT.csv", col_types = cols(`X1` = col_integer()))
DE <- read_csv("DE.csv", col_types = cols(`X1` = col_integer()))
IOL <- read_csv("IOL.csv", col_types = cols(`X1` = col_integer()))
Tackle <- read_csv("Tackle.csv", col_types = cols(`X1` = col_integer()))
TE <- read_csv("TE.csv", col_types = cols(`X1` = col_integer()))
WR <- read_csv("WR.csv", col_types = cols(`X1` = col_integer()))
CB <- read_csv("CB.csv", col_types = cols(`X1` = col_integer()))
S <- read_csv("S.csv", col_types = cols(`X1` = col_integer()))

#Aggregate mean career value per team

ag <- aggregate(`Approx Val CarAV` ~ Tm, data = full_data, FUN = mean)


#intialize as GT table

team_graph1 <- gt(ag) %>%
  
  #Title table
  
  tab_header(title = "Average Pick Value by Team") %>%
  
  #Rename columns
  
  cols_label(Tm = "Team",
             `Approx Val CarAV` = "Average Pick Value")

#Create outcome tables to be used for QB model table titles

outcome_table <- c("Passing Yards Per Game" = "NFL.passing.pg",
                   "Passing TDs Per Game" = "NFL.passtd.pg",
                   "Yards Per Attempt" = "NFLYPA",
                   "Value Per Year" = "ValuePerYear")

#Create outcome tables to be used for RB model table titles

outcome_table1 <- c("NFLYPC" = "NFLYPC",
                    "Rushing Attempts" = "NFL.rushattmpt.pg",
                    "Rushing Yards" = "NFL.rushyrds.pg",
                    "Rushing TDs" = "NFL.rushtd.pg ",
                    "Value Per Year" = "ValuePerYear",
                    "Receptions" = "NFL.receptions.pg",
                    "Receiving Yards" = "NFL.receivingyrds.pg",
                    "Receiving TDs" = "`Receiving TD`",
                    "Yards Per Reception" = "NFLYPR")

#Create outcome tables to be used for WR model table titles

outcome_table2 <- c("NFLYPC" = "NFLYPC",
                    "Rushing Attempts" = "NFL.rushattmpt.pg",
                    "Rushing Yards" = "NFL.rushyrds.pg ",
                    "Rushing TDs" = "NFL.rushtd.pg ",
                    "Value Per Year" = "ValuePerYear",
                    "Receptions" = "NFL.receptions.pg",
                    "Receiving Yards" = "NFL.receivingyrds.pg",
                    "Receiving TDs" = "`Receiving TD`",
                    "Yards Per Reception" = "NFLYPR")






#Initialize UI

ui <- navbarPage(
  
  #Create About page
  
  "Predicting NFL Success from Physical and Production Traits",
                     tabPanel("Plots",
                              fluidPage(theme = shinytheme("cerulean"),
                              h3("Plot of Round and Career Value"),
                              titlePanel("Plots"),
                              
                              #Explain plot
                              
                              p("The following plotshows the relationship between all players and their career values
             by the round they were selected in in their respective NFL drafts. As we see, the first round
             generally produces the highest average career values, which each round subsequently falling in average value "),
                              
                              #Output plot onto page
                              
                              fluidPage(plotOutput("plot")),
                              br(),
                              br(),
                              br(),
                              br(),
                              sidebarPanel(
                                
                                #Out put team model with  average value per pick
                                
                                gt_output("teammodel")),
                              h3("Team Performance: Value per Pick 2016-2018"),
                              p("The table to the left is the average career value per pick for
             each of the 32 NFL teams in the league (The Chargers appear twice due
             to their move to Los Angeles) the data shows that the New Orleans Saints
             had the strongest average drafts, while teams such as the Washington Football
             Team had the worst draft. The NFL draft is a source of life for teams, and
             generally teams that perform well in the draft translate it to the field. 
             The Saints, Colts, and Titans used these drafts to build deep and talented
             rosters, while the drafts of teams such as Washington, Cincinatti, and Minnesota
             have led to deflated seasons. ")
                     )),
                     
                     #Create models tab
                     
                     tabPanel("Models",
                              tabPanel(
                                
                                #Explain regression model
                                
                                h3("Regression of Combine Stats"),
                                p("This regression determines the coefficients of combine performance
           for different positions on average career value. This includes determinants
           such as the 40 yard dash; which measures a players speed running 40 yards,
           the bench press measures the number of reps of 225 pounds a prospect
           can perform without fail, the three cone drill tests player agility 
           and ability to manuever their body around three cones in a preset order,
           the broad jump measurs a prospects explosiveness and horizontal jumping
           ability while the vertical jump determines the height a prospect can jump 
           from a preset position.
           
           
             "),
                                
                                #Create choices for regression of combine stats
                                
                                selectInput(inputId = "Position_Selection",
                                            label = "Choose a position",
                                            choices = c("RB" = "RB", 
                                                        "WR" = "WR", 
                                                        "TE" = "TE", 
                                                        "OT" = "Tackle", 
                                                        "IOL" = "IOL", 
                                                        "DT" = "DT",
                                                        "DE" = "DE", 
                                                        "LB" = "LB",
                                                        "CB" = "CB", 
                                                        "S" = "S"),
                                            selected = "QB"),
                                
                                #Output as results table
                                
                                gt_output("model2"),
                                h3("Analysis"),
                                p("Some interesting takeaways and significant variables include,
              -The 40 yard dash is 90% significant for a WR's value
              -Bench Press is 90% significant for a DT's value
              -The three-cone drill is 95% significant for a Safeties value,
              -Round is 95% significant for all players"),
                                br(),
                                br(),
                                
                                #Create regression of physical traits
                                
                                h3("Regression of Physical Traits"),
                                p("This regression determines the impact of physical traits such as height and 
    weight by position on average career value, the variables include height,
               weight, arm length and hand size"),
                                
                                #Create inout for userrs to select desired position
                                
                                selectInput(inputId = "Position_Selection2",
                                            label = "Choose a position",
                                            choices = c("RB" = "RB", 
                                                        "WR" = "WR", 
                                                        "TE" = "TE", 
                                                        "OT" = "Tackle", 
                                                        "IOL" = "IOL", 
                                                        "DT" = "DT",
                                                        "DE" = "DE", 
                                                        "LB" = "LB",
                                                        "CB" = "CB", 
                                                        "S" = "S"),
                                            selected = "QB"),
                                
                                
                                #Output as data table
                                
                                gt_output("model3")),
                              h3("Analysis"),
                              p("Some interesting takeaways and significant variables include,
              -Hand size is 95% significant for a WR's value
              -Arm Length is 90% significant and negative for a DT's value
              -Weight is 95% significant and negative for a LBs value,
              -Weight is 95% significant and negative for a Safeties value")
                     ),
                     
                     
  #Create dataset summaries
  
  tabPanel("Summary Stats",
           fluidPage(
             tabPanel("Dataset Summaries"),
             sidebarLayout(
               sidebarPanel(
                 
                 #Allow users to pick datasets by position
                 
                 selectInput(inputId = "dataset",
                             label = "Choose a Position:",
                             choices = c( "QB" = "QB",
                                          "RB" = "RB", 
                                          "WR" = "WR", 
                                          "TE" = "TE", 
                                          "OT" = "Tackle", 
                                          "IOL" = "IOL", 
                                          "DT" = "DT",
                                          "DE" = "DE", 
                                          "LB" = "LB",
                                          "CB" = "CB", 
                                          "S" = "S"
                             ))),
               
               mainPanel(verbatimTextOutput("summary"))),
             h3("Data Explanation"),
             p("Each of the positional datasets has been created by restricting 
               the full dataset to applicable variables. The following are some
               descriptions of key variables and common acronyms.
               
               Avg Val Car Avg is a formula that considers the production value
               of players based on their contributions at their respective 
               position, it is the best value to use too standardize player 
               value across positions
               
               YPA is yards per attempt, and AYPA is adjusted yards per attempt
               which considers drops and other stats. Yards per attempt is a passing
               stat that quantifies the average yardage gain achieved per pass 
               attempt
               
               YPC is yards per carry, it is a rushing stat that determines a 
               running back's efficiency
               
               YPR is yards per reception, it is a receiving stat that determines
               how many yards a player gets on average whenn catching the ball
               
               Quarterback rating is a formula that considers interceptions, sacks,
               yards per attempt, completion percentage and more
               
               Completion percentage is the percentage of QB attempts that are completed")
           )),
  
  #Createe plot with career values and round
  
  
  
  #Create tab for QB production
  
  tabPanel("QB Production Model",
             h3("Regression of Quarterbacks prior Stats on NFL Value and Statistics"),
             p("This is a regression of quarterback NFL per game stats based on college
               per game stats, and includes predictors such as college passing yards
               per game, completion percentage, passer rating, number of games played,
               round drafted and adjusted yards per attempt."),
           
             #Create selection input for user to pick variable to regress on
             
             selectInput(inputId = "selection_qb",
                         label = "Choose a variable",
                         choices = c("Passing Yards Per Game" = "NFL.passing.pg",
                                     "Passing TDs Per Game" = "NFL.passtd.pg",
                                     "Yards Per Attempt" = "NFLYPA",
                                     "Value Per Year" = "ValuePerYear"),
                         selected = "NFL.passing.pg"),
           
           #Create selection input for user to pick regressors
           
    selectInput(inputId = "selected_predictor",
               label =  "Choose a regressor",
               choices = c("Round Drafted" = "Rnd",
                           "Passer Rating" = "College.Passer.Rating",
                           "College Completions Per Game" = "college.comp.pg",
                           "College Games" = "College.Games",
                          "College Completion Rate" = "College.Completion.Rate",
                          "College Adjusted Yards Per Attempt" = "College.Adjusted.YPA"),
              multiple = TRUE,
              
              #Input round and college games as the default
              
              selected = c("Rnd",
                           "College.Games")),
    
    #Output as table model
           
           gt_output("model4"),
           h3("Analysis"),
           p("This model uses college statistics such as the round drafter,
             their passer rating (a formula using other stats to standardize
             efficiency), the number of games played in college, and their
             college completion rate, as well as the adjusted yards per attempt
             (number of yards on average per passing attempt. You can choose
             which regressor to use to find its effect on NFL stats such as
             passing yards per game, passing touchdowns per game, yards per 
             attempt and more.")),
  
  
  #Create RB production model tab
  
  tabPanel("RB Production Model",
             h3("Regression of Running Backs prior Stats on NFL Value and Statistics"),
             p("This is a regression of runningback NFL per game stats based on college
               per game stats, and includes predictors such as college rushing yards
               per game, tds per game, college yards per carry, attempts per game and
               receptions, receiving yards, and receiving TDs per game."),
             
             #Create selection input for user to pick variable to regress on
             
             selectInput(inputId = "selection_rb",
                         label = "Choose a variable",
                         choices = c("NFLYPC" = "NFLYPC",
                                     "Rushing Attempts" = "NFL.rushattmpt.pg",
                                     "Rushing Yards" = "NFL.rushyrds.pg ",
                                     "Rushing TDs" = "NFL.rushtd.pg ",
                                     "Value Per Year" = "ValuePerYear",
                                     "Receptions" = "NFL.receptions.pg",
                                     "Receiving Yards" = "NFL.receivingyrds.pg",
                                     "Receiving TDs" = "`Receiving TD`",
                                     "Yards Per Reception" = "NFLYPR"),
                         selected = "NFL.rushyrds.pg"),
           
           #Create selection input for user to pick regressors
           
             selectInput(inputId = "selected_predictor1",
                         label =  "Choose a regressor",
                         choices = c("College Yards Per Carry" = "College.YPC",
                          "College Attempts Per Game" = "college.rushattmpt.pg",
                          "College Rush TD Per Game" = "college.rushtd.pg",
                          "College Rush Yards Per Game" = "college.rushyrds.pg",
                         "Round" = "Rnd",
                         "College Receptions Per Game" = "college.receptions.pg", 
                          "College Receiving TD Per Game" = "college.receivingtd.pg", 
                         "College Receiving Yards Per Game" = "college.receivingyrds.pg"),
                         multiple = TRUE,
                         
                         #Input round and college yards per carry as the default
                         
                         selected = c("Rnd",
                                      "College.YPC")),
           
           #Output as regression table 
           
           gt_output("model5"),
           h3("Analysis"),
           p("This model uses college statistics such as the round drafted,
           the average yards per carry,  attempts, yards and rushing TDs per
           game, as well as receiving stats such as receptions, receiving yards,
           and receiving touchdowns per game. The user can select which
           regressor to use to find its effect on NFL stats such as
               rushing yards per game, rushing touchdowns per game, yards per 
               carry and more. Some takeaways are that college receptions per game
             is positively and significantly associated with receiving yards
             per game.College receiving yards are negatively and significantly
             associated with NFL rushing attempts and rushing yards.  ")
           
),

#Initialize WR production page

tabPanel("WR Production Model",
         h3("Regression of Wide Receivers prior Stats on NFL Value and Statistics"),
         p("This is a regression of Wide Receiver NFL per game stats based on college
               per game stats, and includes predictors such as college receiving 
               yards per game, tds per game, college yards per receptions, 
               receptions per game on NFL receptions, receiving yards, and 
           receiving TDs per game."),
         
         #Create selection input for user to pick variable to regress on
         
         selectInput(inputId = "selection_wr",
                     label = "Choose a variable",
                     choices = c("NFLYPC" = "NFLYPC",
                                 "Rushing Attempts" = "NFL.rushattmpt.pg",
                                 "Rushing Yards" = "NFL.rushyrds.pg ",
                                 "Rushing TDs" = "NFL.rushtd.pg ",
                                 "Value Per Year" = "ValuePerYear",
                                 "Receptions" = "NFL.receptions.pg",
                                 "Receiving Yards" = "NFL.receivingyrds.pg",
                                 "Receiving TDs" = "`Receiving TD`",
                                 "Yards Per Reception" = "NFLYPR"),
                     
                     #Input round and college yards per reception as the default
                     
                     selected = "NFL.YPR"),
         
         #Create selection input for user to pick regressors
         
         selectInput(inputId = "selected_predictor2",
                     label =  "Choose a regressor",
                     choices = c("College Yards Per Reception" = "College.YPR",
                                 "College Attempts Per Game" = "college.rushattmpt.pg",
                                 "College Rush TD Per Game" = "college.rushtd.pg",
                                 "College Rush Yards Per Game" = "college.rushyrds.pg",
                                 "Round" = "Rnd",
                                 "College Receptions Per Game" = "college.receptions.pg", 
                                 "College Receiving TD Per Game" = "college.receivingtd.pg", 
                                 "College Receiving Yards Per Game" = "college.receivingyrds.pg"),
                     multiple = TRUE,
                     
                     #Input round and college yards per carry as the default
                     
                     selected = c("Rnd",
                                  "college.receptions.pg")),
         
         #Output as regression table
         
         gt_output("model6"),
         h3("Analysis"),
         p("This model uses college statistics such as the round drafted,
           the average yards per reception,  receptions, yards and receiving TDs per
           game, as well as receiving stats such as receptions, receiving yards,
           and receiving touchdowns per game. The user can select which
           regressor to use to find its effect on NFL stats such as
           receiving yards per game, receiving touchdowns per game, yards per 
           reception and more. Some takeaways are that college receiving yards
           per game is statistically significant and positive for college yards
           per reception after controlling for receptions and receiving TDs, and 
           college rushing touchdowns are statistically significant for NFL
           rushing TDs")
         
),
#Initialize background page

tabPanel("About",
h3("Project Background and Motivations"),

#Write background motivation

p("This project was started with the hope of finding overrated or 
           underrated variables NFL scouts use to determine a college players 
           likelihood of being successful in the NFL. The findings have included 
           some unnique conclusions, such as a Wide Receiver's 
             40 yard dash time not being linked to his NFL Yards per catch, and 
             many other unintuitive takeaways. The goal is to find he important 
             physical and productive traits that give decision makers a better 
             lens of a successful Professional Football Player"),

#Add data sourcing info

h3("Data Sourcing"),

#Write data sourcing 

p("My data repository can be found here",a("data repo", href = 
                                             "https://github.com/sethf26/shiny.git"), 
  "my full data includes 1370 observations of 165 variables, 
            which have been pieced together and created by myself.  The first collection of data is NFL 
            statistics, which came from pro-football reference. Next is combine 
            and draft statistics which were pulled directly from the NFL's website. 
            For college statistics, I scraped for each of the years I intended to garner data on, 
            then cleaned this data and combined it into one dataset. To standardize 
            for players with different numbers of years in the league, I created 
            over 50 variables quantifying NFL and College statistics on a per game basis"),

#Add further action info

h3("Further Action to be Taken"),
p("In continuation of this project, I plan to continue adding years 
            to this dataset to increase its robustness, and apply non-linear models that will 
              allow me to get a better gauge on characteristics that contribute to a successful NFL career"),

#Talk about myself for abit in the about  me section

h3("About Me"),
p("Seth Filo is a Junior at Harvard Collge studying 
                     Government and Economics with an interest in data science
                     and using it to properly gauge societal issues find logical solutions, 
                     and better predict outcomes. He is originally
                     from Ohio, but has lived in Arizona the past 10 years with a brief detour
             in Alabama for a year, allowing for a unique understanding of 
              everyday America and the issues it faces")
           ))





server <- function(input, output, session) {
  
  
  #Generate numerical summary of datasets
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- get(input$dataset)
    summary(dataset)
  })
  
  #Generate ggplot of value by round
  
  output$plot <- renderPlot({
    ggplot(full_data, mapping = aes(x = Rnd, 
                                    y = `Approx Val CarAV`, 
                                    color = Pos)) +
      geom_point() +
      labs(title = "Average Career Value by Round",
           x = "Round Drafter",
           y = "Average Career Value") +
      theme_classic()
  })
  
  #Generate scores for each team
  
  output
  
  # Show the first "n" observations 
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  #Create regression formula for combine
  
  formula <- reactive({
    as.formula(`Approx Val CarAV` ~ `40 Yard Dash` + `Bench Press` + Rnd + `Broad Jump` + `Three Cone Drill`+`Vertical Jump`)
  }) 
  
  #Input formula with data as selected by user
  
  model_2 <- reactive({
    stan_glm(data = get(input$Position_Selection ),
             formula = formula(),
             refresh = 0,
             family = "gaussian")})
  
  #Create output with table with titles
  
  output$model2 <- render_gt(
    model_2() %>%
      tbl_regression() %>%
      as_gt() %>%
      tab_header(title = paste("Regression of Combine Stats by Position"),
                 subtitle = "The Effect of Various Combine Statistics on Career Value"))
  
  #Physical Traits Model
  
  #Initialize physical traits formula
  
  formula1 <- reactive({
    as.formula(`Approx Val CarAV` ~ Rnd + Height + Weight + `Arm Length` + Hands)
  }) 
  
  #Input formula with data as selected by user
  
  model_3 <- reactive({
    stan_glm(data = get(input$Position_Selection2),
             formula = formula1(),
             refresh = 0,
             family = "gaussian")})
  
  #Output as table with titles
  
  
  output$model3 <- render_gt(
    model_3() %>%
      tbl_regression() %>%
      as_gt() %>%
      tab_header(title = paste("Regression of Physical Traits by Position"),
                 subtitle = "The Effect of Various Physical Traits on Career Value"))
  
  #Input formula with data as selected by user, using paste with the selected
  #input to paste in regressors separated by plus ssigns
  
  formula3 <- reactive({
    as.formula(paste(input$selection_qb,
                     "~ ValuePerYear +", 
                     paste(input$selected_predictor, 
                           collapse =" + ")))
  })
  
  #Intialize linear model using the formula created above
  
  model_4 <- reactive({
    stan_glm(data = QB,
             formula = formula3(),
             refresh = 0,
             family = "gaussian")})
  
  #Print model as a GT regression table
  
  
  output$model4 <- render_gt(
    model_4() %>%
      tbl_regression(intercept = TRUE) %>%
      as_gt() %>%
      
      #Create table headers using the outcome table for QB and input
      
      tab_header(title = paste("Regression of", 
                               names(outcome_table)
                               [outcome_table == input$selection_qb]),
                 subtitle = paste("Predicted changes in", 
                                  names(outcome_table)
                                  [outcome_table == input$selection_qb],
                                  "as various predictors increase")))
    
  #Input formula with data as selected by user, using paste with the selected
  #input to paste in regressors separated by plus signs
    
  formula4 <- reactive({
    as.formula(paste(input$selection_rb,
                     "~ ValuePerYear +", 
                     paste(input$selected_predictor1, 
                           collapse =" + ")))
  })
  
  #Intialize linear model using the formula created above
  
  model_5 <- reactive({
    stan_glm(data = RB,
             formula = formula4(),
             refresh = 0,
             family = "gaussian")})
  
  #Print model as a GT regression table
  
  output$model5 <- render_gt(
    model_5() %>%
      tbl_regression(intercept = TRUE) %>%
      as_gt() %>%
      
      #Create table headers using the outcome table for RB and input
      
      tab_header(title = paste("Regression of", 
                               names(outcome_table1)
                               [outcome_table1 == input$selection_rb]),
                 subtitle = paste("Predicted changes in", 
                                  names(outcome_table1)
                                  [outcome_table1 == input$selection_rb],
                                  "as various predictors increase")))
  
  #Input formula with data as selected by user, using paste with the selected
  #input to paste in regressors separated by plus signs
  
  formula5 <- reactive({
    as.formula(paste(input$selection_wr,
                     "~ ValuePerYear +", 
                     paste(input$selected_predictor2, 
                           collapse =" + ")))
  })
  
  
  #Intialize linear model using the formula created above
  
  model_6 <- reactive({
    stan_glm(data = WR,
             formula = formula5(),
             refresh = 0,
             family = "gaussian")})
  
  #Print model as a GT regression table
  
  output$model6 <- render_gt(
    model_6() %>%
      tbl_regression(intercept = TRUE) %>%
      as_gt() %>%
      
      #Create table headers using the outcome table for RB and input
      
      tab_header(title = paste("Regression of", 
                               names(outcome_table2)
                               [outcome_table2 == input$selection_wr]),
                 subtitle = paste("Predicted changes in", 
                                  names(outcome_table2)
                                  [outcome_table2 == input$selection_wr],
                                  "as various predictors increase")))
  
  
  
  #Render team table
  
  output$teammodel  <- render_gt({
    team_graph1
  })
  
    
  

}

shinyApp(ui = ui,server = server)