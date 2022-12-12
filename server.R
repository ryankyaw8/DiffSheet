
library(tidyverse)
library(gt)
library(readxl)
library(shiny)

men_courses <- read_excel("MenCourses.xlsx")
women_courses <- read_excel("WomenCourses.xlsx")
player_scorecards <- read_excel("PlayerScorecards.xlsx")
course_scorecards_men <- read_excel("CourseScorecardsMen.xlsx")
course_scorecards_women <- read_excel("CourseScorecardsWomen.xlsx")
spring22_men <- read_excel("CLUGolfSpring22.xlsx")
spring22_women <- read.csv("CLUGolfSpring22_Women.csv")

colnames(spring22_women)[1] <- "Player"

spring22_men <- spring22_men %>%
  select(c("Player", "Date", "Golf Course", "Round Type", "Score"))
spring22_men$Date <- as.character(spring22_men$Date)

spring22_women <- spring22_women %>%
  select(c("Player", "Date", "Golf.Course", "Round.Type", "Score"))
colnames(spring22_women) <- c("Player", "Date", "Golf Course", "Round Type", "Score")

spring22 <- rbind(spring22_men, spring22_women)



# test <- read_excel("CLUGolfSpring22.xlsx")

source("visualscode.R")

function(input, output, session){
  # output$scorecard <- render_gt(expr = scorecard(input$name, input$date))
  # output$diffsheet <- renderDataTable(expr = diff("Male"))
  # output$customdiff <- renderDataTable(expr = diff_custom(input$rounds))
  # output$averages <- renderDataTable(expr = averages(x))
  # output$buckets <- renderDataTable(expr = distance_buckets(x))
  
  output$diffsheet <- renderDataTable(expr = diff(input$Gender))
  output$customdiff <- renderDataTable(expr = diff_custom(input$Rounds, input$Gender))
  output$scorecard <- render_gt(expr = scorecard(input$name, input$date))
  output$hole_by_hole <- renderDataTable(expr = hole_by_hole(input$gender2, input$course))
  output$averages <- renderDataTable(expr = averages(input$gender3, input$roundtype))
  output$buckets <- renderDataTable(expr = distance_buckets(input$gender3, input$roundtype))
  output$past_rounds <- renderDataTable(expr = player_analyzer_past_rounds(input$player, input$rounds2))
  output$playerhistory <- renderDataTable(expr = all_player_analyzer_past_rounds(input$player2, input$roundtype2))
  output$summary <- renderDataTable(expr = player_historical_stats((input$player2)))
  
  sgp <- eventReactive(input$sgp_calc, {sgp_calculator(input$hole1_dist, input$hole2_dist, input$hole3_dist, input$hole4_dist, input$hole5_dist, input$hole6_dist, input$hole7_dist, input$hole8_dist, input$hole9_dist,
                                                      input$hole10_dist, input$hole11_dist, input$hole12_dist, input$hole13_dist, input$hole14_dist, input$hole15_dist, input$hole16_dist, input$hole17_dist, input$hole18_dist,
                                                      input$hole1_putt, input$hole2_putt, input$hole3_putt, input$hole4_putt, input$hole5_putt, input$hole6_putt, input$hole7_putt, input$hole8_putt, input$hole9_putt,
                                                      input$hole10_putt, input$hole11_putt, input$hole12_putt, input$hole13_putt, input$hole14_putt, input$hole15_putt, input$hole16_putt, input$hole17_putt, input$hole18_putt,
                                                      input$round_date, input$player_name, input$golf_course, input$round_type)})

  output$sgp <- renderText(sgp())

  output$sgp_rankings <- renderDataTable(expr = sgp_rankings())
  
  
}


