#Data prep



library(readxl)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

#read in post_election data
#SEND THIS FILE TO TFS:)
Q4_data <- read_excel("Q4_data.xls") %>% 
  rename(district_n = district) %>% 
  filter(win_party != "UNDECIDED") %>% 
  #recode AL to 1 s
  mutate(district_n = fct_recode(district_n,
                                 "1" =  "AL" ))



#section off non-congressional races and manipulate them
#this creates data set tracing percent rep_win for elections
Q4_senate_gov <- Q4_data %>% 
  filter(district_n == "sen" | district_n == "gov")%>% 
  mutate(district = str_c(state, district_n, sep = "-"))%>% 
  #calculate percentage rep_win
  mutate(rep_win = 100*(rep_votes - dem_votes)/(dem_votes + rep_votes + other_votes)) %>% 
  select(rep_win, district)




#reformat district code to match other date for post_election data
#calculate rep_advantage in win 
Q4_congress <-  Q4_data %>% 
  filter(district_n != "sen", district_n != "gov", district_n != "AL") %>% 
  #create merged district variable
  mutate(district = str_c(state, district_n, sep = "-")) %>% 
  #calculate percentage rep_win
  mutate(rep_win = 100*(rep_votes - dem_votes)/(dem_votes + rep_votes + other_votes))%>% 
  select(rep_win, district)




#introduce forecast data from pre-election polling, prepare to join by c("district")
#I'm using the data which I had from the midterm, including the congressional district column which I created on the midterm

poll_data <- read_rds("pset7_data.rds")


#calculate the prediction for rep advantage
#this will cause the data to dtop the other variables so I will have to rejoin 
poll_data2 <-  poll_data %>%
  #combine the values for different polling waves
  mutate(wave = fct_collapse(wave,
                             poll = c("1", "2", "3")
  )) %>%
  group_by(house_district, wave) %>% 
  count(response, wt = final_weight)%>% 
  # group_by(response, n) %>% 
  spread(key = response, value = n) %>% 
  #I lost the final_weight variable so I manually calculate the sum below
  mutate(all = sum(Dem,Rep, Und, `3`, `4`,`5`, `6`, na.rm = TRUE)) %>% 
  mutate(rep_advantage = 100*(Rep - Dem)/all)%>% 
  select(house_district, wave, rep_advantage) %>% 
  spread(key = wave, value = rep_advantage)%>% 
  rename(rep_adv = poll) %>% 
  #bring back other poll values which had been dropped when doing the above calculations
  inner_join(poll_data, by = "house_district") %>% 
  #select the necessary variables, including race_eth
  select(house_district, response, rep_adv, race_eth) %>% 
  #filter out race ethnicity responses that were blank
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  separate(house_district, into = c("state", "dist"), sep = 2) %>% 
  #mutate district names to be properly formatted in upper case with dash in between
  mutate(state = str_to_upper(state), district = str_c(state, dist, sep = "-")) %>% 
  select(-dist) 





#join congress data, rep_win is how much the republicans won or lost by while rep_adv is the forecast of what will occur based on polling

all_congress <- inner_join(Q4_congress, poll_data2, by = "district") %>% 
  #find error, gap in percentage points between win margin and forecasted margin in polling
  mutate(error = rep_win - rep_adv, election.type = "congress", accuracy = 100 - abs(error)) 
#I calculated accuracy using the forumula employed by Charlie Olmert of 100 - abs(error)

