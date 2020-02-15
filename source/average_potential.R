library(magrittr)
library(dplyr)   
fifa20 <- read.csv("players_15.csv")

fifa20_potential <- fifa20 %>%
  filter(age < 25) %>%
  arrange(desc(potential)) %>% 
  select(club, potential) %>% 
  group_by(club) %>% 
  arrange(club) %>% 
  mutate(count = n(), total_potentital = sum(potential)) %>% 
  group_by(club) %>% 
  mutate(average_potential = sum(potential)/count) %>% 
  select(club, average_potential) %>% 
  unique()
