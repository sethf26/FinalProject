tabPanel(
  sidebarPanel(
    h3("Regression of Running Backs prior Stats on NFL Value and Statistics"),
    selectInput(inputId = "selection2",
                label = "Choose a position",
                choices = c("NFLYPC" = "RB$NFLYPC ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Rushing Attempts" = "RB$NFL.rushattmpt.pg ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Rushing Yards" = "RB$NFL.rushyrds.pg ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Rushing TDs" = "RB$College.Rushing.TDs ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Value Per Year" = "RB$ValuePerYear ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Receptions" = "RB$college.receptions.pg ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Receiving Yards" = "RB$college.receivingyrds.pg ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Receiving TDs" = "RB$`Receiving TD` ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS",
                            "Yards From Scrimmage" = "RB$College.AVGYFS ~ RB$College.YPC + RB$college.rushattmpt.pg + RB$college.rushtd.pg + RB$College.Rushing.Yards + RB$Rnd + RB$College.Receptions + RB$College.Receiving.TDs + RB$College.Receiving.Yards + RB$College.AVGYFS")
                
    ))),

gt_output("model5")


output$model5 <- render_gt({
  
  
  model_5 <- stan_glm(formula = input$selection2,
                      data = RB,
                      refresh = 0)
  
  model_5 %>%
    tidy %>%
    gt() %>%
    tab_header(title = "Runningbacks NFL Stat predictions from College Stats")
})
