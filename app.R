
#Here is a link to my shiny app https://hahe211.shinyapps.io/Q4_midterm2/

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
  mutate(rep_win = 100*(rep_votes - dem_votes)/(dem_votes + rep_votes + other_votes)) %>% 
  select(rep_win, district)
  


#introduce forecast data from Q3 for congress, prepare to join by c("district")
#still need to write and input rds file!

Q3forQ4 <- read_rds("Q3forQ4.rds")

Q3_forQ4_congress <-  Q3forQ4 %>% 
  group_by(house_district, wave) %>% 
  count(response, wt = final_weight)%>% 
  # group_by(response, n) %>% 
  spread(key = response, value = n) %>% 
  #I lost the final_weight variable so I manually calculate the sum below
  mutate(all = sum(Dem,Rep, Und, `3`, `4`,`5`, `6`, na.rm = TRUE)) %>% 
  mutate(rep_advantage = 100*(Rep - Dem)/all) %>% 
  select(house_district, wave, rep_advantage) %>% 
  spread(key = wave, value = rep_advantage) %>% 
  select(-poll_1) %>% 
  rename(rep_adv = poll_2) %>% 
  separate(house_district, into = c("state", "dist"), sep = 2) %>% 
  #mutate district names to be properly formatted in upper case with dash in between
  mutate(state = str_to_upper(state), district = str_c(state, dist, sep = "-")) %>% 
  select(-state, -dist) 



#introduce data for senate/gov from forecasts data pre-election
#FIRST: read in rds data
#send this file to TFS !!! :)

use_in_Q4_senate <- read_rds("use_in_Q4_senate.rds")

#Next manipulate data so it can be productively joined with post-election data

Q3_forQ4_senate <- use_in_Q4_senate %>% 
  group_by(dist_id, wave) %>% 
  count(response, wt = final_weight)%>% 
  # group_by(response, n) %>% 
  spread(key = response, value = n) %>% 
  #I lost the final_weight variable so I manually calculate the sum below
  mutate(all = sum(Dem,Rep, Und, `4`,`5`, `6`, na.rm = TRUE)) %>% 
  mutate(rep_advantage = 100*(Rep - Dem)/all) %>% 
  select(dist_id, wave, rep_advantage) %>% 
  spread(key = wave, value = rep_advantage) %>% 
  mutate(rep_adv =  sum(`2`,`3`, na.rm = TRUE)) %>% 
  select(dist_id, rep_adv) %>% 
  #reformat district names to match post-election data 
  separate(dist_id, into = c("state", "dist"), sep = 2)%>% 
  #mutate district names to be properly formatted in upper case with dash in between
  mutate(state = str_to_upper(state), district = str_c(state, dist, sep = "-")) %>% 
  select(-state, -dist) 





#join congress data  

all_congress <- inner_join(Q4_congress, Q3_forQ4_congress, by = "district") %>% 
  #find error, gap in percentage points between win margin and forecasted margin in polling
   mutate(error = rep_win - rep_adv, election.type = "congress")


 #join non-congress data
all_non_congress <- inner_join(Q3_forQ4_senate, Q4_senate_gov, by = "district") %>% 
#find error, gap in percentage points between win margin and forecast
  mutate(error = rep_win - rep_adv, election.type = "senate_gov")


#final graphic looks at the difference between prediction and reality by state
all_data <- bind_rows(all_congress, all_non_congress) #%>% 
     #mutate(error = as.factor(error))



# Define UI for the application
ui <- fluidPage(
  titlePanel("Examining the Forecasting Error for Fall 2018 Elections"),
  h2("How did the Pew Polling Forecasts differ from Outcomes?"), 
  em("I chose a task slightly different from the assignment: to look at how the prediction error differed by district"),
  sidebarLayout(
    sidebarPanel(
      numericInput("size", "Point size", 1, 1),
      radioButtons("colour", "Point colour",
                   choices = c("blue", "red", "green", "black"
                               ))
     #selectInput("election.types", "Election.types",
          #       choices = levels(all_data$election.type),
                #  multiple = TRUE,
                #  selected = "congress")
     ),
  
    mainPanel(
      plotOutput("plot", width = 500, height = 500),
      plotOutput("plot2", width = 500, height = 500)
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
     #data <- subset(all_data,
            #       election.type %in% input$election.types)
                   
    p <- ggplot(all_non_congress, aes(district, error)) +
      geom_point(size = input$size, col = input$colour) +
      ggtitle("Forecast Error in Senate and Gov Elections")+
      xlab("Senate or Governor Race") + ylab("Forecast Error in Republican Win Margin")
    
    p
    
  })
  
  output$plot2 <- renderPlot({
 
    
    q <- ggplot(all_congress, aes(district, error)) +
      geom_point(size = input$size, col = input$colour) +
      ggtitle("Forecast Error in Congressional Elections")+
      xlab("Congressional District") + ylab("Forecast Error in Republican Win Margin")
    
    q
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)





