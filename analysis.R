
library(tidyverse)
library(readxl)

current <- read_excel("PlayerScorecards.xlsx")
past <- read_excel("CLUGolfSpring22.xlsx")

current_tierra <- current %>%
  filter(Gender == "Male" & Course == "Tierra") %>%
  select(c("Date", "Name", "Course", "Total"))
  
current_olivas <- current %>%
  filter(Gender == "Male" & Course == "Olivas Links") %>%
  select(c("Date", "Name", "Course", "Total"))

past_tierra <- past %>%
  filter(`Golf Course` == "Tierra Rejada") %>%
  select(c("Date", "Player", "Golf Course", "Score"))
colnames(past_tierra) <- c("Date", "Name", "Course", "Total")

past_olivas <- past %>%
  filter(`Golf Course` == "Olivas Links") %>%
  select(c("Date", "Player", "Golf Course", "Score"))
colnames(past_olivas) <- c("Date", "Name", "Course", "Total")

tierra <- rbind(current_tierra, past_tierra)
olivas <- rbind(current_olivas, past_olivas)

hist(tierra$Total)
hist(olivas$Total)

tierra_avg <- mean(tierra$Total)
olivas_avg <- mean(olivas$Total)

will_tierra <- tierra %>%
  filter(Name == "Will Moser")
jerry_tierra <- tierra %>%
  filter(Name == "Jerry Singh")

redlands <- player_scorecards %>%
  filter(Course == "Redlands")

mean(redlands$Total)

olivas <- player_scorecards %>%
  filter(Course == "Olivas Links" & RoundType == "Tournament" & Gender == "Male")
mean(olivas$Total)  

mean(tierra$Total)












