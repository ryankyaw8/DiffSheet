
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

test <- read_excel("CLUGolfSpring22.xlsx")

source("visualscode.R")

# ui <- navbarPage("CLU Golf 22-23 Stats Explorer",
#                  tabPanel("Diff Sheet",
#                           mainPanel(tags$h1("Overall Diff Sheet"),
#                                     dataTableOutput(outputId = "diffsheet"),
#                                     br(),
#                                     tags$h1("Customizable Diff Sheet"),
#                                     sliderInput("rounds", label = "Select Number of Rounds to Calculate Diff", min = 1, max = 15, value = 4),
#                                     dataTableOutput(outputId = "customdiff"))),
#
#                  tabPanel("Scorecards",
#                           mainPanel(tags$h1("Scorecards Display"),
#                                     selectInput("name", "Select Player Name", 
#                                                 choices = c(unique(player_scorecards$Name))),
#                                     selectInput("date", "Select Date",
#                                                 choices = c(unique(player_scorecards$Date))),
#                                     gt_output(outputId = "scorecard"))),
#                  
#                  tabPanel("Stats",
#                           mainPanel(tags$h1("Average Per Round Statistics"),
#                                     dataTableOutput(outputId = "averages"),
#                                     br(),
#                                     tags$h1("Scoring by Hole Distance Bucket"),
#                                     dataTableOutput(outputId = "buckets"))))


ui <- navbarPage("CLU Golf 22-23 Stats Explorer",
                 tabPanel("Diff Sheet",
                          mainPanel(
                            h2("Diff Sheet"),
                            h5("Differential is Score - Course Rating"),
                            h5("DIFFERENTIAL ACCOUNTS FOR COURSE DIFFICULTY"),
                            h5("Men Ratings: Olivas - 72.5, Tierra - 73.8, Redlands - 70.3"),
                            h5("Women Ratings: Olivas - 74, San Dimas - 73.9, Willamette - 72.9"),
                            selectInput("Gender", "Select Gender", choices = c("Male", "Female")),
                            br(),
                            dataTableOutput(outputId = "diffsheet"),
                            br(),
                            h2("Custom Diff Sheet"),
                            h5("Choose the number of rounds using this slider below"),
                            br(),
                            sliderInput("Rounds", "Number of Rounds", min = 1, max = 10, value = 2),
                            br(),
                            dataTableOutput(outputId = "customdiff"),
                            br(),
                            h2("Player Past Rounds"),
                            selectInput("player", "Select Player", choices = c(unique(player_scorecards$Name))),
                            sliderInput("rounds2", "Number of Rounds", min = 1, max = 10, value = 3),
                            br(),
                            dataTableOutput(outputId = "past_rounds"),
                            br(),
                            h2("Scorecards"),
                            br(),
                            selectInput("name", "Select Name", choices = c(unique(player_scorecards$Name))),
                            selectInput("date", "Select Date", choices = c(unique(player_scorecards$Date))),
                            gt_output(outputId = "scorecard")
                          )),

                 tabPanel("Course Stats",
                          mainPanel(tags$h1("Hole by Hole Statistics"),
                                    br(),
                                    selectInput("gender2", "Select Gender", choices = c("Male", "Female")),
                                    selectInput("course", "Select Course", choices = c(unique(player_scorecards$Course))),
                                    br(),
                                    dataTableOutput(outputId = "hole_by_hole"))),
                 
                 tabPanel("Player Stats",
                          mainPanel(selectInput("gender3", "Select Gender", choices = c("Male", "Female")),
                                    selectInput("roundtype", "Select Round Type", choices = c("All", "Tournament", "Qualifying")),
                                    tags$h1("Average Per Round Statistics"),
                                    dataTableOutput(outputId = "averages"),
                                    br(),
                                    tags$h1("Scoring by Hole Distance Bucket"),
                                    dataTableOutput(outputId = "buckets"))),
                 
                 tabPanel("Player Historical Analysis",
                          mainPanel(selectInput("player2", "Select Player", choices = c(unique(player_scorecards$Name))),
                                    selectInput("roundtype2", "Select Round Type", choices = c("All", "Qualifying", "Tournament")),
                                    br(),
                                    tags$h2("Player Round History"),
                                    dataTableOutput(outputId = "playerhistory"),
                                    br(),
                                    tags$h2("Scoring Average by Round Type"),
                                    dataTableOutput(outputId = "summary"))),
                 
                 tabPanel("Strokes Gained Putting",
                          mainPanel(h2("Strokes Gained Putting Calculator"),
                                    selectInput("player_name", "Select Name", choices = c(unique(player_scorecards$Name))),
                                    dateInput("round_date", "Enter Date of Round Played", format = "yyyy-mm-dd"),
                                    selectInput("golf_course", "Select the Golf Course Played", choices = c(unique(player_scorecards$Course))),
                                    selectInput("round_type", "Select the type of Round Played", choices = c("Tournament","Qualifying", "Practice")),
                                    h3("Hole 1"),
                                    textInput("hole1_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole1_putt", "Number of Putts"),
                                    h3("Hole 2"),
                                    textInput("hole2_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole2_putt", "Number of Putts"),
                                    h3("Hole 3"),
                                    textInput("hole3_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole3_putt", "Number of Putts"),
                                    h3("Hole 4"),
                                    textInput("hole4_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole4_putt", "Number of Putts"),
                                    h3("Hole 5"),
                                    textInput("hole5_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole5_putt", "Number of Putts"),
                                    h3("Hole 6"),
                                    textInput("hole6_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole6_putt", "Number of Putts"),
                                    h3("Hole 7"),
                                    textInput("hole7_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole7_putt", "Number of Putts"),
                                    h3("Hole 8"),
                                    textInput("hole8_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole8_putt", "Number of Putts"),
                                    h3("Hole 9"),
                                    textInput("hole9_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole9_putt", "Number of Putts"),
                                    h3("Hole 10"),
                                    textInput("hole10_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole10_putt", "Number of Putts"),
                                    h3("Hole 11"),
                                    textInput("hole11_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole11_putt", "Number of Putts"),
                                    h3("Hole 12"),
                                    textInput("hole12_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole12_putt", "Number of Putts"),
                                    h3("Hole 13"),
                                    textInput("hole13_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole13_putt", "Number of Putts"),
                                    h3("Hole 14"),
                                    textInput("hole14_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole14_putt", "Number of Putts"),
                                    h3("Hole 15"),
                                    textInput("hole15_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole15_putt", "Number of Putts"),
                                    h3("Hole 16"),
                                    textInput("hole16_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole16_putt", "Number of Putts"),
                                    h3("Hole 17"),
                                    textInput("hole17_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole17_putt", "Number of Putts"),
                                    h3("Hole 18"),
                                    textInput("hole18_dist", "Distance of 1st Putt", placeholder = "Enter distance in feet"),
                                    textInput("hole18_putt", "Number of Putts"),
                                    br(),
                                    actionButton("sgp_calc", "Calculate SGP"),
                                    br(),
                                    br(),
                                    br(),
                                    textOutput(outputId = "sgp", container = tags$h3),
                                    h2("Strokes Gained Putting Rankings"),
                                    br(),
                                    h3("Men's Rankings"),
                                    dataTableOutput(outputId = "sgp_rankings"))))








