library(magrittr)
library(dplyr)   
fifa15 <- read.csv("data/players_15.csv")
fifa16 <- read.csv("data/players_16.csv")
fifa17 <- read.csv("data/players_17.csv")
fifa18 <- read.csv("data/players_18.csv")
fifa19 <- read.csv("data/players_19.csv")
fifa20 <- read.csv("data/players_20.csv")
club_id <- read.csv("data/club_id.csv")
country <- read.csv("data/Fifa_Teams_Leagues_Country.csv")
teams_leagues <- read.csv("data/teams_and_leagues.csv")
total <- rbind(fifa15, fifa16, fifa17, fifa18, fifa19, fifa20)
teams_leagues <- teams_leagues %>% 
  unique()
club_id <- club_id %>% 
  mutate(url = selection1_id, club = selection1_name) %>% 
  select(url,club)
leagues_clubs <- merge(club_id, leagues, by = "url")
total <- merge(total, leagues_clubs, by = "club", all.x = FALSE)
# use the latest dataset to filter the players below 25 years old
fifa_potential <- total %>%
  filter(age < 25) %>%
  arrange(desc(potential)) %>% 
  select(club, potential, league_name) %>% 
  group_by(club, league_name) %>% 
  arrange(club) %>% 
  mutate(count = n(), total_potentital = sum(potential)) %>% 
  group_by(club, league_name) %>% 
  mutate(average_potential =  total_potentital / count) %>% 
  select(club, average_potential, league_name) %>%
  unique()

fifa20_wages <- total %>%
  filter(age < 25) %>% 
  select(club, wage_eur, league_name) %>% 
  group_by(club, league_name) %>% 
  mutate(count = n(), total_wage = sum(wage_eur)) %>% 
  mutate(average_wage_per_club = total_wage / count) %>%
  select(club, average_wage_per_club,league_name) %>% 
  unique()


fifa20_value <- total %>%
  filter(age < 25) %>% 
  select(club, value_eur, league_name) %>% 
  group_by(club, league_name) %>% 
  mutate(count = n(), total_value = sum(value_eur)) %>% 
  mutate(average_value_per_club = total_value / count) %>%
  select(club, average_value_per_club,league_name) %>% 
  unique()

write.csv(fifa_potential,"average_potential.csv", row.names = FALSE)
write.csv(fifa20_wages,"average_wages.csv", row.names = FALSE)
write.csv(fifa20_value,"average_value.csv", row.names = FALSE)

