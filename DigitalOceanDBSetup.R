
library(RMySQL)

db <- dbConnect(RMySQL::MySQL(),
                host = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                dbname = "xxxxxxxxxxxx",
                user = "xxxxxxxxxxxxxx",
                password = "xxxxxxxxxxxxxxx",
                port = xxxxx)

#### CLU SGP Table

# clu_sgp <- data.frame(Round_ID = 1,
#                       Date = "2022-12-04",
#                       Golf_Course = "Tierra",
#                       Player_Name = "Ryan Kyaw",
#                       Round_Type = "Qualifying",
#                       SGP = 0.512)

dbSendQuery(db, "CREATE TABLE clu_sgp (round_id INT PRIMARY KEY, 
            round_date DATE,golf_course VARCHAR(100), 
            player_name VARCHAR(100), 
            round_type VARCHAR(100), 
            sgp DECIMAL(4, 3));")

dbListTables(db)

dbReadTable(db, "clu_sgp")

record <- 1
date <- '2022-01-01'
course <- "Test"
player <- "Test Player"
type <- "Test Type"
sgp <- 0.1

query <- paste0("INSERT INTO clu_sgp VALUES(", record, 
                ", '", date, "'", 
                ", '", course, "'",
                ", '", player, "'",
                ", '", type, "'",
                ", ", sgp, ")")

add_row <- dbSendQuery(db, query)

dbDisconnect(db)

dbReadTable(db, "clu_sgp")


#### Pool Table

dbSendQuery(db, "CREATE TABLE pool_teams (team_name VARCHAR(100) PRIMARY KEY, 
            tier1 VARCHAR(100),
            tier2 VARCHAR(100), 
            tier3 VARCHAR(100), 
            tier4 VARCHAR(100), 
            tier5 VARCHAR(100));")

dbReadTable(db, "pool_teams")















