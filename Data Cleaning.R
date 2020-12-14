library(tidyverse)
library(tidyr)
library(rvest)
library(rlist)
library(XML)
library(httr)
library(readxl)
library(taRifx)
library(rstanarm)
library(readr)
library(tidymodels)


# select the years that you want

years <- 2016:2018 

#creates an empty dataset

college_data <- c() 

# This lets us modify the URL text to include the years


drafts <- paste0("https://www.pro-football-reference.com/years/", years, "/draft.htm") 

for (j in 1:length(drafts)) {
  
  page <- read_html(drafts[j])
  
  # get the CSS nodes and extract link text
  
  links <- page %>% 
    html_nodes("a") %>% 
    html_text() 
  
  # get the CSS nodes and extract link text
  
  urls <- page %>%
    html_nodes("a") %>% 
    html_attr("href") 
  
  # making a dataset of URLS to filter out unwanted info
  
  url_data <- data.frame(links, urls) 
  
  # filtering only the college stats links
  
  college <- url_data %>%
    filter(links == "College Stats") 
  
  # reads in their college stats page
  
  for (i in 1:length(college$urls)) {
    
    player <- read_html(college$urls[i]) 
    
    #reads the players name
    
    name <- player %>%
      html_nodes("h1 span") %>% 
      html_text()
    
    # reads the top table.
    
    tables <- player %>%
      html_table()
    
    # Makes the table into a dataset
    
    d <- as.data.frame(tables) %>% 
      mutate(player = name) %>%
      
      # creates a variable for draft year
      
      mutate(draft_year = years[j]) %>%
      slice(-1)
    
    college_data <- bind_rows(college_data, d)
    
    
  }
  
  
}

#Read in datasets and add the year the player was drafted

NFL_2016_label <- read_excel("raw_data/NFL_2016_label.xlsx", 
                             skip = 3)
NFL_2016 <- NFL_2016_label%>%
  mutate(year = 2016)

#Separate out backspace to be able to join data

NFL_2016$Player <- word(NFL_2016$Player, 1,2, sep=" ")

#Read in datasets and add the year the player was drafted

NFL_2017_label <- read_excel("raw_data/NFL_2017_label.xlsx", 
                             skip = 3)
NFL_2017 <- NFL_2017_label%>%
  mutate(year = 2017)

#Separate out backspace to be able to join data

NFL_2017$Player <- word(NFL_2017$Player, 1,2, sep=" ")

#Read in datasets and add the year the player was drafted


NFL_2018_label <- read_excel("raw_data/NFL_2018_label.xlsx", 
                             skip = 3)
NFL_2018 <- NFL_2018_label%>%
  mutate(year = 2018)

#Separate out backspace to be able to join data

NFL_2018$Player <- word(NFL_2018$Player, 1,2, sep=" ")

#Join datasets into a full stat dataset with all 3 years


NFL_Full_stat <- join(NFL_2016,NFL_2017,by = "year", type = "full")
NFL_Full_stat <- join(NFL_Full_stat,NFL_2018,by = "year", type = "full")

#Read in combine data adding the year

Combine_2016 <- read_excel("raw_data/Combine_2016.xlsx")
Combine_2016 <- Combine_2016%>%
  mutate(year = 2016)

#Read in combine data adding the year

Combine_2017 <- read_excel("raw_data/Combine_2017.xlsx")
Combine_2017 <- Combine_2017%>%
  mutate(year = 2017)

#Read in combine data adding the year

Combine_2018 <- read_excel("raw_data/Combine_2018.xlsx")
Combine_2018 <- Combine_2018%>%
  mutate(year = 2018)

#Combine Combine datasets into 1 frame with all 3 years

Combine_Full_stat <- join(Combine_2016,Combine_2017,by = "year", type = "full")
Combine_Full_stat <- join(Combine_Full_stat,Combine_2018,by = "year", type = "full")

#Join the NFL and Combine datasets by the players name

Combine_NFL_Full <- join(NFL_Full_stat, Combine_Full_stat, by = "Player", type = "full")


#Read in college dataset created from scraping

college_data <- read_excel("College_data.xlsx")

#Separate datasets to make all columns except first few numeric

college_data1 <- college_data %>%
  select(Var.1:Var.6, player)

college_data2 <- college_data %>%
  select(Passing:Kick.Ret.3, -player)

#Make all columns in split set numeric with Lapply

college_data2[] <- lapply(college_data2, function(x) as.numeric(as.character(x)))

#Rebind the columns into one dataset

college_data <- cbind(college_data1,college_data2)


#Rename columns for future clarity

college_data <- college_data %>%
  rename("Year1" = "Var.1",
         "College" = "Var.2",
         "Conference" = "Var.3",
         "Class.Year" = "Var.4",
         "Position" = "Var.5",
         "College.Games" = "Var.6",
         "College.Completions" = "Passing",
         "College.Passing.Attempts" = "Passing.1",
         "College.Completion.Rate" = "Passing.2",
         "College.Passing.Yards" = "Passing.3",
         "College.Yards.Per.Attempt" = "Passing.4",
         "College.Adjusted.YPA" = "Passing.5",
         "College.Passing.Touchdowns" = "Passing.6",
         "College.Passing.Interceptions" = "Passing.7",
         "College.Passer.Rating" = "Passing.8",
         "College.Solo.tackle" = "Tackles",
         "College.Assist.Tackles" = "Tackles.1",
         "College.Total.Tackles" = "Tackles.2",
         "College.TFL" = "Tackles.3",
         "College.Sacks" = "Tackles.4",
         "College.Interceptions" = "Def.Int",
         "College.Int.Yards" = "Def.Int.1",
         "College.Int.Tds" = "Def.Int.3",
         "College.Pass.Defensed" = "Def.Int.4",
         "College.Fumble.Recoveries" = "Fumbles",
         "College.Forced.Fumbles" = "Fumbles.3",
         "College.Receptions" = "Receiving",
         "College.Receiving.Yards" = "Receiving.1",
         "College.YPR" = "Receiving.2",
         "College.Receiving.TDs" = "Receiving.3",
         "College.Rushing.Attempts" = "Rushing",
         "College.Rushing.Yards" = "Rushing.1",
         "College.YPC" = "Rushing.2",
         "College.Rushing.TDs" = "Rushing.3",
         "College.Total.Plays" = "Scrimmage",
         "College.Scrimmage.Yards" = "Scrimmage.1",
         "College.AVGYFS" = "Scrimmage.2",
         "College.Total.Touchdowns" = "Scrimmage.3",
         "Player" = "player"
  )

#Create several per game statistics for uniformity
#Divide statistics by number of games played

college_3 <- college_data %>%
  filter(Year1 == "Career") %>%
  mutate(college.passing.pg = College.Passing.Yards/College.Games,
         college.att.pg = College.Passing.Attempts/College.Games,
         college.comp.pg = College.Completions/College.Games,
         college.passtd.pg = College.Passing.Touchdowns/College.Games,
         college.passint.pg = College.Passing.Interceptions/College.Games,
         college.solotackles.pg = College.Solo.tackle/College.Games,
         college.asst.pg = College.Assist.Tackles/ College.Games,
         college.totaltck.pg = College.Total.Tackles/College.Games,
         college.TFL.pg = College.TFL/College.Games,
         college.sack.pg = College.Sacks/College.Games,
         college.int.pg = College.Interceptions/College.Games,
         college.passdefensed.pg = College.Pass.Defensed/College.Games,
         college.ff.pg = College.Forced.Fumbles/College.Games,
         college.receptions.pg = College.Receptions/College.Games,
         college.receivingyrds.pg = College.Receiving.Yards/College.Games,
         college.receivingtd.pg = College.Receiving.TDs/College.Games,
         college.rushattmpt.pg = College.Rushing.Attempts/College.Games,
         college.rushyrds.pg = College.Rushing.Yards/College.Games,
         college.rushtd.pg = College.Rushing.TDs/College.Games,
         College.scrimyards.pg = College.Scrimmage.Yards/College.Games,
         college.tottd.pg = College.Total.Touchdowns/College.Games)


#Join the Combine and NFL dataset with the college dataset

full_data <- full_join(Combine_NFL_Full, college_3, by = "Player")

#Split data to make certain combine stats numeric

full_data1 <- full_data %>%
  select(`40 Yard Dash`, 
         `Bench Press`, 
         `Vertical Jump`, 
         `Broad Jump`,
         `Three Cone Drill`,
         `20 Yard Shuttle`,
         `60 Yard Shuttle`)

full_data2 <- full_data %>%
  select(-`40 Yard Dash`, 
         -`Bench Press`, 
         -`Vertical Jump`, 
         -`Broad Jump`,
         -`Three Cone Drill`,
         -`20 Yard Shuttle`,
         -`60 Yard Shuttle`)

#Make variables in split dataset numeric

full_data1[] <- lapply(full_data1, function(x) as.numeric(as.character(x)))

#Rejoin the dataset

full_data <- cbind(full_data1,full_data2)

#Create full data for future loading

full_data <- read_csv("fulldata.csv")

#Add per game statistics for NFL stats

full_data <- full_data %>%
  mutate(NFLYPC =`Rushing Yds`/`Rushing Att`,
         NFLYPR = `Receiving Yds`/Receptions,
         NFLYPA = `Passing Yds`/`Passing Att`,
         NFLINTTD = `Passing TD`/`Passing Int`,
         ValuePerYear = `Approx Val CarAV`/(2020 - year),
         NFL.passing.pg = `Passing Yds`/G,
         NFL.att.pg = `Passing Att`/G,
         NFL.comp.pg = `Passing Cmp`/G,
         NFL.passtd.pg = `Passing TD`/G,
         NFL.passint.pg = `Passing Int`/G,
         NFL.solotackles.pg = `Solo Tackle`/G,
         NFL.sack.pg = Sk/G,
         NFL.int.pg = Int/G,
         NFL.receptions.pg = Receptions/G,
         NFL.receivingyrds.pg = `Receiving Yds`/G,
         NFL.receivingtd.pg = `Receiving TD`/G,
         NFL.rushattmpt.pg = `Rushing Att`/G,
         NFL.rushyrds.pg = `Rushing Yds`/G,
         NFL.rushtd.pg = `Rushing TD`/G
  )


#Create QB dataset using only applicable variables

QB <- full_data %>%
  filter(Pos == "QB") %>%
  select(Rnd,
         NFLINTTD,
         NFLYPA,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         NFL.att.pg,
         NFL.comp.pg,
         NFL.passint.pg,
         NFL.passtd.pg,
         NFL.passing.pg,
         `Passing Cmp`,
         `Passing Att`,
         `Passing Yds`,
         `Passing TD`,
         `Passing Int`,
         `Rushing Att`,
         `Rushing Yds`,
         `College/Univ`,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         College.Completions,
         College.Passing.Attempts,
         College.Completion.Rate,
         College.Passing.Yards,
         College.Yards.Per.Attempt,
         College.Adjusted.YPA,
         College.Passing.Touchdowns,
         College.Passing.Interceptions,
         College.Passer.Rating,
         college.passing.pg,
         college.att.pg,
         college.comp.pg,
         college.passtd.pg,
         college.passint.pg,
         ValuePerYear
  )

#Create RB dataset using only applicable variables

RB <- full_data %>%
  filter(Pos == "RB") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         NFL.receptions.pg,
         NFL.receivingyrds.pg,
         NFL.receivingtd.pg, 
         NFL.rushattmpt.pg,
         NFL.rushyrds.pg,
         NFL.rushtd.pg ,
         College.Games,
         `Rushing Att`,
         `Rushing Yds`,
         Receptions,
         `Receiving Yds`,
         `Receiving TD`,
         `College/Univ`,
         year,
         Grade,
         NFLYPC,
         College.Rushing.Attempts,
         College.Rushing.Yards,
         College.Rushing.TDs,
         College.Receptions,
         College.Receiving.Yards,
         College.Receiving.TDs,
         NFLYPR,
         College.YPR,
         College.YPC,
         College.Total.Plays,
         College.Total.Tackles,
         College.Total.Touchdowns,
         College.Scrimmage.Yards,
         College.AVGYFS,
         college.receptions.pg,
         college.receivingyrds.pg,
         college.receivingtd.pg,
         college.rushattmpt.pg,
         college.rushyrds.pg,
         college.rushtd.pg,
         College.scrimyards.pg,
         college.tottd.pg,
         ValuePerYear
  )

#Create TE dataset using only applicable variables

TE <- full_data %>%
  filter(Pos == "TE") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `Rushing Att`,
         `Rushing Yds`,
         Receptions,
         `Receiving Yds`,
         `Receiving TD`,
         `College/Univ`,
         year,
         Grade,
         NFLYPC,
         College.Rushing.Attempts,
         College.Rushing.Yards,
         College.Rushing.TDs,
         College.Receptions,
         College.Receiving.Yards,
         College.Receiving.TDs,
         NFL.receptions.pg,
         NFL.receivingyrds.pg,
         NFL.receivingtd.pg, 
         NFL.rushattmpt.pg,
         NFL.rushyrds.pg,
         NFL.rushtd.pg ,
         NFLYPR,
         College.YPR,
         College.YPC,
         College.Total.Plays,
         College.Total.Tackles,
         College.Total.Touchdowns,
         College.Scrimmage.Yards,
         College.AVGYFS,
         college.receptions.pg,
         college.receivingyrds.pg,
         college.receivingtd.pg,
         college.rushattmpt.pg,
         college.rushyrds.pg,
         college.rushtd.pg,
         College.scrimyards.pg,
         college.tottd.pg,
         ValuePerYear
  )

#Create WR dataset using only applicable variables

WR <- full_data %>%
  filter(Pos == "WR") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `Rushing Att`,
         `Rushing Yds`,
         Receptions,
         `Receiving Yds`,
         `Receiving TD`,
         `College/Univ`,
         year,
         Grade,
         NFLYPC,
         NFL.receptions.pg,
         NFL.receivingyrds.pg,
         NFL.receivingtd.pg, 
         NFL.rushattmpt.pg,
         NFL.rushyrds.pg,
         NFL.rushtd.pg ,
         College.Rushing.Attempts,
         College.Rushing.Yards,
         College.Rushing.TDs,
         College.Receptions,
         College.Receiving.Yards,
         College.Receiving.TDs,
         NFLYPR,
         College.YPR,
         College.YPC,
         College.Total.Plays,
         College.Total.Tackles,
         College.Total.Touchdowns,
         College.Scrimmage.Yards,
         College.AVGYFS,
         college.receptions.pg,
         college.receivingyrds.pg,
         college.receivingtd.pg,
         college.rushattmpt.pg,
         college.rushyrds.pg,
         college.rushtd.pg,
         College.scrimyards.pg,
         college.tottd.pg,
         ValuePerYear
  )

##Create Tackle dataset using only applicable variables

Tackle <- full_data %>%
  filter(Pos == "T") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         `College/Univ`,
         year,
         Grade,
         ValuePerYear
  )

#Create Interior Oline dataset using only applicable variables

IOL <- full_data %>%
  filter(Pos == c("G","C")) %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         `College/Univ`,
         year,
         Grade,
         ValuePerYear
  )

#Create DT dataset using only applicable variables

DT <- full_data %>%
  filter(Pos == "DT") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `College/Univ`,
         year,
         Grade,
         `Solo Tackle`,
         Int,
         Sk,
         NFL.solotackles.pg,
         NFL.sack.pg,
         NFL.int.pg,
         College.Solo.tackle,
         College.Assist.Tackles,
         College.TFL,
         College.Sacks,
         College.Interceptions,
         College.Int.Yards,
         College.Int.Tds,
         College.Pass.Defensed,
         College.Fumble.Recoveries,
         College.Forced.Fumbles,
         college.solotackles.pg,
         college.asst.pg,
         college.totaltck.pg,
         college.TFL.pg,
         college.sack.pg,
         college.passdefensed.pg,
         college.ff.pg,
         college.int.pg,
         ValuePerYear)

#Create DE dataset using only applicable variables

DE <- full_data %>%
  filter(Pos == "DE") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `College/Univ`,
         year,
         Grade,
         `Solo Tackle`,
         Int,
         Sk,
         NFL.solotackles.pg,
         NFL.sack.pg,
         NFL.int.pg,
         College.Solo.tackle,
         College.Assist.Tackles,
         College.TFL,
         College.Sacks,
         College.Interceptions,
         College.Int.Yards,
         College.Int.Tds,
         College.Pass.Defensed,
         College.Fumble.Recoveries,
         College.Forced.Fumbles,
         college.solotackles.pg,
         college.asst.pg,
         college.totaltck.pg,
         college.TFL.pg,
         college.sack.pg,
         college.passdefensed.pg,
         college.ff.pg,
         college.int.pg,
         ValuePerYear)

#Create LB dataset using only applicable variables


LB <- full_data %>%
  filter(Pos == c("OLB","ILB")) %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `College/Univ`,
         year,
         Grade,
         `Solo Tackle`,
         Int,
         Sk,
         NFL.solotackles.pg,
         NFL.sack.pg,
         NFL.int.pg,
         College.Solo.tackle,
         College.Assist.Tackles,
         College.TFL,
         College.Sacks,
         College.Interceptions,
         College.Int.Yards,
         College.Int.Tds,
         College.Pass.Defensed,
         College.Fumble.Recoveries,
         College.Forced.Fumbles,
         college.solotackles.pg,
         college.asst.pg,
         college.totaltck.pg,
         college.TFL.pg,
         college.sack.pg,
         college.passdefensed.pg,
         college.ff.pg,
         college.int.pg,
         ValuePerYear)


#Create CB dataset using only applicable variables

CB <- full_data %>%
  filter(Pos == "CB") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `College/Univ`,
         year,
         Grade,
         `Solo Tackle`,
         NFL.solotackles.pg,
         NFL.sack.pg,
         NFL.int.pg,
         Int,
         Sk,
         College.Solo.tackle,
         College.Assist.Tackles,
         College.TFL,
         College.Sacks,
         College.Interceptions,
         College.Int.Yards,
         College.Int.Tds,
         College.Pass.Defensed,
         College.Fumble.Recoveries,
         College.Forced.Fumbles,
         college.solotackles.pg,
         college.asst.pg,
         college.totaltck.pg,
         college.TFL.pg,
         college.sack.pg,
         college.passdefensed.pg,
         college.ff.pg,
         college.int.pg,
         ValuePerYear)

#Create S dataset using only applicable variables

S <- full_data %>%
  filter(Pos == "S") %>%
  select(Rnd,
         Pick,
         Tm,
         Player,
         Pos,
         Age,
         To,
         `Approx Val CarAV`,
         G,
         year,
         Grade,
         Height,
         `Arm Length`,
         Weight,
         Hands,
         `40 Yard Dash`,
         `Bench Press`,
         `Vertical Jump`,
         `Broad Jump`,
         `Three Cone Drill`,
         `Hands Rank`,
         `40 Rank`,
         `Bench Rank`,
         `Vertical Rank`,
         `Broad Rank`,
         `3 Cone Rank`,
         `20 Shuttle Rank`,
         `60 Shuttle Rank`,
         College.Games,
         `College/Univ`,
         year,
         Grade,
         `Solo Tackle`,
         Int,
         Sk,
         NFL.solotackles.pg,
         NFL.sack.pg,
         NFL.int.pg,
         College.Solo.tackle,
         College.Assist.Tackles,
         College.TFL,
         College.Sacks,
         College.Interceptions,
         College.Int.Yards,
         College.Int.Tds,
         College.Pass.Defensed,
         College.Fumble.Recoveries,
         College.Forced.Fumbles,
         college.solotackles.pg,
         college.asst.pg,
         college.totaltck.pg,
         college.TFL.pg,
         college.sack.pg,
         college.passdefensed.pg,
         college.ff.pg,
         college.int.pg,
         ValuePerYear)


#Write  positional datasets into memory

  
  setwd("~/Documents/Gov 50/FinalProject")
  
 write.csv(QB, "QB.csv")
 
 write.csv(RB, "RB.csv")
 
 write.csv(WR, "WR.csv")
 
 write.csv(TE, "TE.csv")
 
 write.csv(Tackle, "Tackle.csv")
 
 write.csv(S, "S.csv")
 
 write.csv(LB, "LB.csv")
 
 write.csv(DE, "DE.csv")
 
 write.csv(DT, "DT.csv")
 
 write.csv(IOL, "IOL.csv")
 
 write.csv(CB, "CB.csv")
 
 write.csv(full_data, "fulldata.csv")
 
  
  
    