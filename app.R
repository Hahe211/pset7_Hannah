

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
   #select the necessary variables
  select(house_district, response, rep_adv) %>% 
  #filter out race ethnicity responses that were blank
  #filter(race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  separate(house_district, into = c("state", "dist"), sep = 2) %>% 
  #mutate district names to be properly formatted in upper case with dash in between
  mutate(state = str_to_upper(state), district = str_c(state, dist, sep = "-")) %>% 
  select(-dist) 





#join congress data, rep_win is how much the republicans won or lost by while rep_adv is the forecast of what will occur based on polling

all_congress <- inner_join(Q4_congress, poll_data2, by = "district") %>% 
  #find error, gap in percentage points between win margin and forecasted margin in polling
   mutate(error = rep_win - rep_adv, election.type = "congress", accuracy = 100 - abs(error)) 
  #I calculated accuracy using the forumula employed by Charlie Olmert of 100 - abs(error)
    


#now that I've created the data frame, my next step will be to plot accuracy (y value)
#against rep_win, rep_adv with the ability to toggle by state to see if there's a relationships 


# Define UI for the application
ui <- fluidPage(
  
  titlePanel("Examining the Forecasting Accuracy for Fall 2018 Elections"),
 
  # Sidebar - inspired by Ms. Gaytons sidebar with the idea to toggle states on and off
  sidebarLayout(
    sidebarPanel(
      selectInput("x",
                  "X-axis:",
                  c(`Polled Republican Advantage` = "rep_adv",
                    `Actual Republican Advantage` = "rep_win")),
      #checkboxInput("line", label = "Add linear model"),
      htmlOutput("see_table"),
      htmlOutput("regression_table"),
      h2("Choose states to display"),
      checkboxGroupInput("state", "States to show:",
                         c("California" = "CA",
                           "Florida" = "FL",
                           "Illinois" = "IL",
                           "Kansas" = "KS",
                           "Michigan" = "MI",
                           "North Carolina" = "NC",
                           "New Jersey" = "NJ",
                           "New York" = "NY",
                           "Pennsylvania" = "PA",
                           "Texas" = "TX",
                           "Virginia" = "VA"), selected = all_congress$state),
      actionButton("selectall", label="Select/Deselect all")
    ),
    
  
  
    mainPanel(
      h3("Summary of Findings"),
      h5("tbd"),
      plotOutput("distPlot")
    )
    )
    )


  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    observe({
      if (input$selectall > 0) {
        if (input$selectall %% 2 == 0){
          updateCheckboxGroupInput(session=session, 
                                   inputId="state",
                                   choices = list("California" = "CA",
                                                  "Florida" = "FL",
                                                  "Illinois" = "IL",
                                                  "Kansas" = "KS",
                                                  "Michigan" = "MI",
                                                  "North Carolina" = "NC",
                                                  "New Jersey" = "NJ",
                                                  "New York" = "NY",
                                                  "Pennsylvania" = "PA",
                                                  "Texas" = "TX",
                                                  "Virginia" = "VA"),
                                   selected = all_congress$state)
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   inputId="state",
                                   choices = list("California" = "CA",
                                                  "Florida" = "FL",
                                                  "Illinois" = "IL",
                                                  "Kansas" = "KS",
                                                  "Michigan" = "MI",
                                                  "North Carolina" = "NC",
                                                  "New Jersey" = "NJ",
                                                  "New York" = "NY",
                                                  "Pennsylvania" = "PA",
                                                  "Texas" = "TX",
                                                  "Virginia" = "VA"),
                                   selected = c())
          
        }}
    })
    
    output$distPlot <- renderPlot({
      
      filteredData <- reactive ({
        df <- all_congress[all_congress$state %in% input$state,]
      })
      # 
      # for(i in 1:length(variables)) {
      #   if(input$x == variables[i]) {
      #     x_axis <- labels[i]
      #     break
      #   }
      #   else {
      #     i <- i + 1
      #   }
      # }
      
      if(input$line == TRUE) {
        ggplot(filteredData(), aes_string(x = input$x, y = "error", col = "state")) + geom_jitter() + 
          geom_smooth(inherit.aes = FALSE, aes_string(x = input$x, y = "error"), method = "lm") + 
          labs(x = x_axis, y = "Percent Error", color = "State")
      } 
      else {
        ggplot(filteredData(), aes_string(x = input$x, y = "error", color = "state")) + geom_jitter() + 
          labs(x = x_axis, y = "Percent Error", color = "State")
      }
    })
    
    output$see_table <- renderUI ({
      if(input$line == TRUE) {
        h5("Please consult the regression table below to see if the relationship is statistically significant:")
      }
    })
    
    output$regression_table <- renderUI({
      filteredData <- reactive ({
        df <- all_congress[all_congress$state %in% input$state,]
      })
      
      if(input$line == TRUE) {
        if(length(input$state) > 0) {
          model <- reactive({ form <- as.formula( paste( "error ~", paste(names(filteredData())[names(filteredData()) %in% input$x], collapse="+")))
          lm(form, data=filteredData())
          })
          HTML(stargazer(model(), type = "html"))
        }
        else {
          h5("No states selected.")
        }
      }
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)