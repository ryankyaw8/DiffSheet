
# Back End Functions for Shiny App

library(tidyverse)
library(gt)
library(readxl)
library(dplyr)
library(RMySQL)

men_courses <- read_excel("MenCourses.xlsx")
women_courses <- read_excel("WomenCourses.xlsx")
player_scorecards <- read_excel("PlayerScorecards.xlsx")
course_scorecards_men <- read_excel("CourseScorecardsMen.xlsx")
course_scorecards_women <- read_excel("CourseScorecardsWomen.xlsx")
spring22_men <- read_excel("CLUGolfSpring22.xlsx")
spring22_women <- read.csv("CLUGolfSpring22_Women.csv")
pga_sg <- read_excel("pga_tour_baselines.xlsx")

colnames(spring22_women)[1] <- "Player"

spring22_men <- spring22_men %>%
  select(c("Player", "Date", "Golf Course", "Round Type", "Score"))
spring22_men$Date <- as.character(spring22_men$Date)

spring22_women <- spring22_women %>%
  select(c("Player", "Date", "Golf.Course", "Round.Type", "Score"))
colnames(spring22_women) <- c("Player", "Date", "Golf Course", "Round Type", "Score")

spring22 <- rbind(spring22_men, spring22_women)


# test <- read_excel("CLUGolfSpring22.xlsx")

# Diff Sheet 

diff <- function(gender){
  
  if (gender == "Male"){
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == gender)
    player_scorecards_filter$Differential <- player_scorecards_filter$Total - 
      men_courses$Rating[match(player_scorecards_filter$Course, men_courses$Course)]
  } else {
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Female")
    player_scorecards_filter$Differential <- player_scorecards_filter$Total - 
      women_courses$Rating[match(player_scorecards_filter$Course, women_courses$Course)]
  }
  
  player_scorecards_tourn <- player_scorecards_filter %>%
    filter(RoundType == "Tournament")
  
  player_scorecards_tourn_diff <- aggregate(player_scorecards_tourn$Differential,
                                       list(player_scorecards_tourn$Name), mean)
  colnames(player_scorecards_tourn_diff) <- c("Player", "Tourn_Diff")
  
  player_scorecards_tourn_scores <- aggregate(player_scorecards_tourn$Total,
                                       list(player_scorecards_tourn$Name), mean)
  colnames(player_scorecards_tourn_scores) <- c("Player", "Tourn_Scoring_Avg")
  
  
  # Overall Diff
  diff_sheet_diffs <- aggregate(player_scorecards_filter$Differential, 
                                list(player_scorecards_filter$Name), mean)
  colnames(diff_sheet_diffs) <- c("Player", "Overall_Differential")
  diff_sheet_diffs$Overall_Differential <- round(diff_sheet_diffs$Overall_Differential, 3)
  
  diff_sheet_scores <- aggregate(player_scorecards_filter$Total, 
                                 list(player_scorecards_filter$Name), mean)
  colnames(diff_sheet_scores) <- c("Player", "Overall_Scoring_Avg")
  diff_sheet_scores$Overall_Scoring_Avg <- round(diff_sheet_scores$Overall_Scoring_Avg, 3)
  
  diff_sheet_rounds <- aggregate(player_scorecards_filter$Date, 
                                 list(player_scorecards_filter$Name), length)
  colnames(diff_sheet_rounds) <- c("Player", "Number_of_Rounds")
  
  
  # Last_4_Diff
  x <- 4
  players <- unique(player_scorecards_filter$Name)
  diff_sheet_4 <- data.frame(Name = c(), Differential = c(), Scoring_Avg = c())
  for(player in players){
    
    player_rounds <- player_scorecards_filter %>%
      filter(Name == player)
    
    index <- nrow(player_rounds) - x
    if(index < 0){
      diff_x <- NA
      scores_x <- NA
      
      tmp <- data.frame(Name = c(player), Differential = c(diff_x), Scoring_Avg = c(scores_x))
    } else {
      player_rounds_x <- player_rounds[(index+1):nrow(player_rounds), ]
      
      diff_x <- mean(na.omit(player_rounds_x$Differential))
      scores_x <- mean(na.omit(player_rounds_x$Total))
      
      tmp <- data.frame(Name = c(player), Differential = c(diff_x), Scoring_Avg = c(scores_x))
    }
    
    diff_sheet_4 <- rbind(tmp, diff_sheet_4)
  }
  
  colnames(diff_sheet_4) <- c("Player", "Last_4_Diff", "Last_4_Scr_Avg")
  
  diff_sheet <- diff_sheet_4 %>%
    left_join(diff_sheet_diffs, by = "Player") %>%
    left_join(diff_sheet_scores, by = "Player") %>%
    left_join(player_scorecards_tourn_diff, by = "Player") %>%
    left_join(player_scorecards_tourn_scores, by = "Player")
  
  diff_sheet <- diff_sheet[order(diff_sheet$Last_4_Diff), ]
  
  return(diff_sheet)

}

test <- diff("Male")


# Customizable diff sheet

diff_custom <- function(lastxrounds, gender){
  
  if (gender == "Male"){
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == gender)
    player_scorecards_filter$Differential <- player_scorecards_filter$Total - 
      men_courses$Rating[match(player_scorecards_filter$Course, men_courses$Course)]
  } else {
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Female")
    player_scorecards_filter$Differential <- player_scorecards_filter$Total - 
      women_courses$Rating[match(player_scorecards_filter$Course, women_courses$Course)]
  }
  
  # Last_4_Diff
  x <- lastxrounds
  players <- unique(player_scorecards_filter$Name)
  diff_sheet_x <- data.frame(Name = c(), Differential = c(), Scoring_Avg = c())
  for(player in players){
    
    player_rounds <- player_scorecards_filter %>%
      filter(Name == player)
    
    index <- nrow(player_rounds) - x
    if(index < 0){
      diff_x <- NA
      scores_x <- NA
      
      tmp <- data.frame(Name = c(player), Differential = c(diff_x), Scoring_Avg = c(scores_x))
    } else {
      player_rounds_x <- player_rounds[(index+1):nrow(player_rounds), ]
      
      diff_x <- mean(na.omit(player_rounds_x$Differential))
      scores_x <- mean(na.omit(player_rounds_x$Total))
      
      tmp <- data.frame(Name = c(player), Differential = c(diff_x), Scoring_Avg = c(scores_x))
    }
    
    diff_sheet_x <- rbind(tmp, diff_sheet_x)
  }
  
  diff_sheet_x <- diff_sheet_x[order(diff_sheet_x$Differential), ]
  
  diff_col_title <- paste0("Last_", lastxrounds, "_Diff")
  score_col_title <- paste0("Last_", lastxrounds, "_Scr_Avg")
  
  colnames(diff_sheet_x) <- c("Player", diff_col_title, score_col_title)
  
  return(diff_sheet_x)
  
}

test <- diff_custom(3, "Male")


# Create the scorecard visual
# User Input: Player Name, Date

# testname <- "Derek Hahn"
# testdate <- "2022-08-01"

# scorecard("Derek Hahn", "2022-08-01")

scorecard <- function(testname, testdate) {
  
  gender_check <- player_scorecards[which(player_scorecards$Name == testname)[1], 3]
  if (gender_check$Gender[1] == "Male"){
    course_scorecards <- course_scorecards_men
  } else {
    course_scorecards <- course_scorecards_women
  }
  
  ind_scorecards <- player_scorecards %>%
    filter(Name == testname)
  
  ind_scorecards$Date <- as.character(ind_scorecards$Date)
  
  dis_scorecard <- ind_scorecards %>%
    filter(Date == testdate)
  
  scores <- as.numeric(dis_scorecard[1, 7:27])
  
  column_names <- colnames(course_scorecards)

  course_card <- course_scorecards %>%
    select(c(which(grepl(dis_scorecard$Course[1], colnames(course_scorecards)))))
  
  course_card <- data.frame(cbind(course_card, scores))
  
  title <- paste(testname, testdate, "Scorecard From", dis_scorecard$Course[1])
  
  colnames(course_card) <- c("Hole", "Yards", "Par", "Score")
  
  test_scorecard <- course_card %>%
    gt() %>%
    tab_header(title = title) %>%
    tab_style(style = list(cell_fill(color = "red"), cell_text(color = "white")),
              locations = cells_body(columns = Score, 
                                     rows = Score == Par - 1)) %>%
    tab_style(style = list(cell_fill(color = "chocolate"), cell_text(color = "white")),
              locations = cells_body(columns = Score,
                                     rows = Score == Par + 1)) %>%
    tab_style(style = list(cell_fill(color = "deeppink"), cell_text(color = "white")),
              locations = cells_body(columns = Score,
                                     rows = Score <= Par - 2)) %>%
    tab_style(style = list(cell_fill(color = "black"), cell_text(color = "white")),
              locations = cells_body(columns = Score,
                                     rows = Score >= Par + 2)) %>%
    tab_style(style = list(cell_fill(color = "white"), cell_text(color = "black")),
              locations = cells_body(columns = Score,
                                     rows = Hole == "Out" | Hole == "In" | Hole == "Total"))
    
  
  return(test_scorecard)
  
}

# Hole by Hole Statistics

hole_by_hole <- function(gender, course){
  
  if(gender == "Male"){
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Male" & Course == course)
    course_scorecards <- course_scorecards_men
  } else {
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Female" & Course == course)
    course_scorecards <- course_scorecards_women
  }
  
  course_card <- course_scorecards %>%
    select(c(which(grepl(course, colnames(course_scorecards)))))
  
  colnames(course_card) <- c("Hole", "Yards", "Par")
  
  course_card$Hole <- as.numeric(course_card$Hole)
  course_card <- course_card[course_card$Hole %in% 1:18, ]
  
  # course_card$`Olivas Links Holes` <- as.numeric(course_card$`Olivas Links Holes`)
  # course_card <- course_card[course_card$`Olivas Links Holes` %in% 1:18, ]
  
  player_scorecards_filter_2 <- player_scorecards_filter %>%
    select(c(7:15, 17:25))
  
  avg_scores <- colMeans(player_scorecards_filter_2)
  
  course_card <- cbind(course_card, avg_scores)
  
  course_card$Avg_To_Par <- round(course_card$avg_scores - course_card$Par, 3)
  course_card$Rank <- floor(rank(-course_card$Avg_To_Par))
  
  colnames(course_card) <- c("Hole", "Yards", "Par", "Avg", "Avg_To_Par", "Rank")
  
  # hole_by_hole_stats <- course_card %>%
  #   gt() %>%
  #   tab_header(title = md("**Olivas Hole by Hole Averages**")) %>%
  #   fmt_number(columns = c(Avg, Avg_To_Par), decimals = 3)
  
  # return(hole_by_hole_stats)
  
  course_card$Avg <- round(course_card$Avg, 3)
  
  return(course_card)
  
}

test <- hole_by_hole("Male", "Redlands")




# Getting player statistics 

# Averages 

averages <- function(gender, type){
  
  if(type == "Tournament"){
    player_scorecards <- player_scorecards %>%
      filter(RoundType == "Tournament")
  } else if (type == "Qualifying"){
    player_scorecards <- player_scorecards %>%
      filter(RoundType == "Qualifying")
  }
  
  player_averages <- data.frame(Name = c(),
                                Rounds = c(),
                                ScoringAverage = c(),
                                Birdies = c(),
                                Bogeys = c(),
                                Others = c(),
                                Par3 = c(),
                                Par4 = c(),
                                Par5 = c())
  
  if(gender == "Male"){
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Male")
    course_scorecards <- course_scorecards_men
  } else {
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Female")
    course_scorecards <- course_scorecards_women
  }
  
  players <- unique(player_scorecards_filter$Name)
  
  for (player in players){
    
    subset <- player_scorecards_filter %>%
      filter(Name == player)
    
    rounds <- nrow(subset)
    
    birdies <- 0
    bogeys <- 0
    others <- 0
    par3 <- 0
    par4 <- 0
    par5 <- 0
    
    Par3_Counter <- 0
    Par4_Counter <- 0
    Par5_Counter <- 0
    
    for (i in 1:nrow(subset)){
      
      scores <- as.numeric(subset[i, c(7:15, 17:25)])
      
      course_card <- course_scorecards %>%
        select(c(which(grepl(subset$Course[i], colnames(course_scorecards)))))
      
      pars <- pull(course_card, 3)[c(1:9, 11:19)]
      
      for (j in 1:length(scores)){
        
        if (scores[j] <= pars[j] - 1){
          birdies <- birdies + 1
        } else if (scores[j] == pars[j] + 1){
          bogeys <- bogeys + 1
        } else if (scores[j] >= pars[j] + 2){
          others <- others + 1
        }
        
        if (pars[j] == 3){
          par3 <- par3 + scores[j]
          Par3_Counter <- Par3_Counter + 1
        } else if (pars[j] == 4){
          par4 <- par4 + scores[j]
          Par4_Counter <- Par4_Counter + 1
        } else if (pars[j] == 5){
          par5 <- par5 + scores[j]
          Par5_Counter <- Par5_Counter + 1
        }
        
        
      }

      
    }
    
    tmp <- data.frame(Name = c(player), Rounds = c(rounds), ScoringAverage = c(mean(subset$Total)),
                      Birdies = c(birdies/rounds), Bogeys = c(bogeys/rounds), Others = c(others/rounds),
                      Par3 = c(par3/Par3_Counter), Par4 = c(par4/Par4_Counter), Par5 = c(par5/Par5_Counter))
    
    player_averages <- rbind(tmp, player_averages)
    
  }
  
  return(player_averages)
  
}

test <- averages("Male", "Qualifying")

# Averages for different hole buckets 

distance_buckets <- function(gender, type){
  
  if(type == "Tournament"){
    player_scorecards <- player_scorecards %>%
      filter(RoundType == "Tournament")
  } else if (type == "Qualifying"){
    player_scorecards <- player_scorecards %>%
      filter(RoundType == "Qualifying")
  }
  
  hole_buckets <- data.frame(Name = c(),
                                Rounds = c(),
                                ScoringAverage = c(),
                                "100-150" = c(),
                                "150-200" = c(),
                                "200-250" = c(),
                                "250-300" = c(),
                                "300-350" = c(),
                                "350-400" = c(),
                                "400-450" = c(),
                                "450-500" = c(),
                                "500-550" = c(),
                                "550-600" = c(),
                                "600+" = c())
  
  if(gender == "Male"){
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Male")
    course_scorecards <- course_scorecards_men
  } else {
    player_scorecards_filter <- player_scorecards %>%
      filter(Gender == "Female")
    course_scorecards <- course_scorecards_women
  }
  
  players <- unique(player_scorecards_filter$Name)
  
  for (player in players){
    
    subset <- player_scorecards_filter %>%
      filter(Name == player)
    
    rounds <- nrow(subset)
    
    cat1 <- 0
    cat2 <- 0
    cat3 <- 0
    cat4 <- 0
    cat5 <- 0
    cat6 <- 0
    cat7 <- 0
    cat8 <- 0
    cat9 <- 0
    cat10 <- 0
    cat11 <- 0
    
    cat1_Counter <- 0
    cat2_Counter <- 0
    cat3_Counter <- 0
    cat4_Counter <- 0
    cat5_Counter <- 0
    cat6_Counter <- 0
    cat7_Counter <- 0
    cat8_Counter <- 0
    cat9_Counter <- 0
    cat10_Counter <- 0
    cat11_Counter <- 0
    
    for (i in 1:nrow(subset)){
      
      scores <- as.numeric(subset[i, c(7:15, 17:25)])
      
      course_card <- course_scorecards %>%
        select(c(which(grepl(subset$Course[i], colnames(course_scorecards)))))
      
      yards <- pull(course_card, 2)[c(1:9, 11:19)]
      
      for (j in 1:length(scores)){
        
        if (yards[j] < 150){
          cat1 <- cat1 + scores[j]
          cat1_Counter <- cat1_Counter + 1
        } else if (yards[j] >= 150 & yards[j] < 200){
          cat2 <- cat2 + scores[j]
          cat2_Counter <- cat2_Counter + 1
        } else if (yards[j] >= 200 & yards[j] < 250){
          cat3 <- cat3 + scores[j]
          cat3_Counter <- cat3_Counter + 1
        } else if (yards[j] >= 250 & yards[j] < 300){
          cat4 <- cat4 + scores[j]
          cat4_Counter <- cat4_Counter + 1
        } else if (yards[j] >= 300 & yards[j] < 350){
          cat5 <- cat5 + scores[j]
          cat5_Counter <- cat5_Counter + 1
        } else if (yards[j] >= 350 & yards[j] < 400){
          cat6 <- cat6 + scores[j]
          cat6_Counter <- cat6_Counter + 1
        } else if (yards[j] >= 400 & yards[j] < 450){
          cat7 <- cat7 + scores[j]
          cat7_Counter <- cat7_Counter + 1
        } else if (yards[j] >= 450 & yards[j] < 500){
          cat8 <- cat8 + scores[j]
          cat8_Counter <- cat8_Counter + 1
        } else if (yards[j] >= 500 & yards[j] < 550){
          cat9 <- cat9 + scores[j]
          cat9_Counter <- cat9_Counter + 1
        } else if (yards[j] >= 550 & yards[j] < 600){
          cat10 <- cat10 + scores[j]
          cat10_Counter <- cat10_Counter + 1
        } else if (yards[j] >= 600){
          cat11 <- cat11 + scores[j]
          cat11_Counter <- cat11_Counter + 1
        }
        
      }
      
    }
    
    # counters <- c(cat1_Counter, cat2_Counter, cat3_Counter, cat4_Counter, cat5_Counter, cat6_Counter,
    #               cat7_Counter, cat8_Counter, cat9_Counter, cat10_Counter, cat11_Counter)
    # for (i in 1:length(counters)){
    #   
    #   if (counters[i] == 0){
    #     counters[i] = 1
    #   }
    #   
    # }
    
    
    tmp <- data.frame(Name = c(player), Rounds = c(rounds), ScoringAverage = c(mean(subset$Total)),
                      "100-150" = c(cat1/cat1_Counter), "150-200" = c(cat2/cat2_Counter), "200-250" = c(cat3/cat3_Counter),
                      "250-300" = c(cat4/cat4_Counter), "300-350" = c(cat5/cat5_Counter), "350-400" = c(cat6/cat6_Counter),
                      "400-450" = c(cat7/cat7_Counter), "450-500" = c(cat8/cat8_Counter), "500-550" = c(cat9/cat9_Counter),
                      "550-600" = c(cat10/cat10_Counter), "600+" = c(cat11/cat11_Counter))
    
    hole_buckets <- rbind(tmp, hole_buckets)
    
  }
  
  return(hole_buckets)
  
}

# Player Analyzer

player_analyzer_past_rounds <- function(player, rounds){
  
  subset <- player_scorecards %>%
    filter(Name == player)
  
  subset_rev_1 <- as.data.frame(t(subset))
  subset_rev_2 <- rev(subset_rev_1)
  subset_rev <- head(as.data.frame(t(subset_rev_2)), rounds)
  
  player_rounds <- subset_rev %>%
    select(c("Date", "Course", "RoundType", "Total"))
  
  return(player_rounds)
  
}

test <- player_analyzer_past_rounds("Tanner Stewart", 4)

all_player_analyzer_past_rounds <- function(player, roundtype) {
  
  if(roundtype == "All"){
    
    subset1 <- player_scorecards %>%
      filter(Name == player)
    
    subset_rev_1 <- as.data.frame(t(subset1))
    subset_rev_2 <- rev(subset_rev_1)
    subset_rev <- as.data.frame(t(subset_rev_2))
    
    subset_rev$Season <- "22-23"
    
    player_rounds1 <- subset_rev %>%
      select(c("Date", "Course", "RoundType", "Total", "Season"))
    
    subset2 <- spring22 %>%
      filter(Player == player)
    
    player_rounds2 <- subset2 %>%
      select(c("Date", "Golf Course", "Round Type", "Score"))
    colnames(player_rounds2) <- c("Date", "Course", "RoundType", "Total")
    
    player_rounds2$Season <- "21-22"
    
    player_rounds2$Date <- as.character(player_rounds2$Date)
    
    player_rounds <- rbind(player_rounds1, player_rounds2)
    
    return(player_rounds)
    
  } else if(roundtype == "Qualifying"){
    
    subset1 <- player_scorecards %>%
      filter(Name == player & RoundType == roundtype)
    
    subset_rev_1 <- as.data.frame(t(subset1))
    subset_rev_2 <- rev(subset_rev_1)
    subset_rev <- as.data.frame(t(subset_rev_2))
    
    subset_rev$Season <- "22-23"
    
    player_rounds1 <- subset_rev %>%
      select(c("Date", "Course", "RoundType", "Total", "Season"))
    
    subset2 <- spring22 %>%
      filter(Player == player & `Round Type` == roundtype)
    
    player_rounds2 <- subset2 %>%
      select(c("Date", "Golf Course", "Round Type", "Score"))
    colnames(player_rounds2) <- c("Date", "Course", "RoundType", "Total")
    
    player_rounds2$Season <- "21-22"
    
    player_rounds2$Date <- as.character(player_rounds2$Date)
    
    player_rounds <- rbind(player_rounds1, player_rounds2)
    
    return(player_rounds)
    
  } else if(roundtype == "Tournament"){
    
    subset1 <- player_scorecards %>%
      filter(Name == player & RoundType == roundtype)
    
    subset_rev_1 <- as.data.frame(t(subset1))
    subset_rev_2 <- rev(subset_rev_1)
    subset_rev <- as.data.frame(t(subset_rev_2))
    
    subset_rev$Season <- "22-23"
    
    player_rounds1 <- subset_rev %>%
      select(c("Date", "Course", "RoundType", "Total", "Season"))
    
    subset2 <- spring22 %>%
      filter(Player == player & `Round Type` == roundtype)
    
    player_rounds2 <- subset2 %>%
      select(c("Date", "Golf Course", "Round Type", "Score"))
    colnames(player_rounds2) <- c("Date", "Course", "RoundType", "Total")
    
    player_rounds2$Season <- "21-22"
    
    player_rounds2$Date <- as.character(player_rounds2$Date)
    
    player_rounds <- rbind(player_rounds1, player_rounds2)
    
    return(player_rounds)
    
  }
           
  
}

all_player_analyzer_past_rounds("JP Guimaraes", "All")

player_historical_stats <- function(player){
  
  subset1 <- player_scorecards %>%
    filter(Name == player)
  
  subset_rev_1 <- as.data.frame(t(subset1))
  subset_rev_2 <- rev(subset_rev_1)
  subset_rev <- as.data.frame(t(subset_rev_2))
  
  subset_rev$Season <- "22-23"
  
  player_rounds1 <- subset_rev %>%
    select(c("Date", "Course", "RoundType", "Total", "Season"))
  
  subset2 <- spring22 %>%
    filter(Player == player)
  
  player_rounds2 <- subset2 %>%
    select(c("Date", "Golf Course", "Round Type", "Score"))
  colnames(player_rounds2) <- c("Date", "Course", "RoundType", "Total")
  
  player_rounds2$Season <- "21-22"
  
  player_rounds2$Date <- as.character(player_rounds2$Date)
  
  player_rounds <- rbind(player_rounds1, player_rounds2)
  
  player_rounds$Total <- as.numeric(player_rounds$Total)
  
  summary_data <- aggregate(player_rounds$Total,
                            list(player_rounds$RoundType,
                                 player_rounds$Season),
                            mean)
  
  colnames(summary_data) <- c("RoundType", "Season", "Avg")
  
  return(summary_data)
  
  
}

player_historical_stats("JP Guimaraes")

sgp_calculator <- function(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8, 
                           dist9, dist10, dist11, dist12, dist13, dist14, dist15, 
                           dist16, dist17, dist18,
                           putt1, putt2, putt3, putt4, putt5, putt6, putt7, putt8,
                           putt9, putt10, putt11, putt12, putt13, putt14, putt15, 
                           putt16, putt17, putt18, date, player, course, type) {
  
  distances <- c(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8, 
                 dist9, dist10, dist11, dist12, dist13, dist14, dist15, 
                 dist16, dist17, dist18)
  putts <- c(putt1, putt2, putt3, putt4, putt5, putt6, putt7, putt8,
             putt9, putt10, putt11, putt12, putt13, putt14, putt15, 
             putt16, putt17, putt18)
  
  distances <- as.numeric(distances)
  putts <- as.numeric(putts)
  
  distances <- distances[!is.na(distances)]
  putts <- putts[!is.na(putts)]
  
  
  sg_vector <- c()
  
  for(i in 1:length(distances)){
    
    baseline <- pga_sg$Putt[match(distances[i], pga_sg$Distance_ft)]
    strokes <- putts[i]
    
    sg_vector <- append(sg_vector, baseline - strokes)
  }
  
  sgp <- sum(sg_vector)
  
  mydb <- dbConnect(RMySQL::MySQL(),
                  host = "rk-first-mysql-cluster-do-user-13028418-0.b.db.ondigitalocean.com",
                  dbname = "defaultdb",
                  user = "doadmin",
                  password = "AVNS_pmapXHtmecypq-48A2U",
                  port = 25060)
  
  current <- fetch(dbSendQuery(mydb, "SELECT * FROM clu_sgp"))
  
  record <- current[nrow(current), 1] + 1
  
  query <- paste0("INSERT INTO clu_sgp VALUES(", record, 
                  ", '", date, "'", 
                  ", '", course, "'",
                  ", '", player, "'",
                  ", '", type, "'",
                  ", ", sgp, ")")
  
  add_row <- dbSendQuery(mydb, query)
  
  dbDisconnect(mydb)
  
  message <- paste("Strokes Gained Putting =", sgp)

  return(message)
  
}

sgp_rankings <- function(){
  
  mydb <- dbConnect(RMySQL::MySQL(),
                  host = "rk-first-mysql-cluster-do-user-13028418-0.b.db.ondigitalocean.com",
                  dbname = "defaultdb",
                  user = "doadmin",
                  password = "AVNS_pmapXHtmecypq-48A2U",
                  port = 25060)
  
  current <- fetch(dbSendQuery(mydb, "SELECT * FROM clu_sgp"))
  
  dbDisconnect(mydb)
  
  current_ranked <- current[order(-current$sgp), ]
  
  top3 <- current_ranked[1:3, ]
  
  
  return(top3)
  
  
}







