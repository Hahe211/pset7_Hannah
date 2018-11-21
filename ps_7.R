library(tidyverse)
library(dplyr)
library(shiny)
library(fs)
library(reshape2)

# 
# table <- read_rds("all_congress.rds") %>% 
#   select(rep_win, rep_adv, accuracy, district) %>% 
#   distinct(district, state, rep_win, rep_adv, accuracy, race_eth) %>% 
#   select(-district)


table <- read_rds("all_congress.rds") %>% 
  mutate(race_eth = fct_collapse(race_eth,
                             "White" = "White",
                              "Black" = "Black", 
                              "Asian" = "Asian", 
                               "Other" = "Other",
                             "Hispanic" = "Hispanic"
                            
  )) %>% 
  distinct(rep_win,accuracy, race_eth) 
#create groupings by race_eth within a district   


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "Republic Win Margin vs Polling Forecasting Accuracy filtered by Race/Ethnicity"),
      numericInput("size", "Point size", 2, 2),
       checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("colour", "Point colour",
                   choices = c("blue", "red", "green", "black")),
      # Add a race_eth dropdown selector
      selectInput("race_eth", "race_eth",
                  choices = levels(table$race_eth),
                  multiple = TRUE,
                  selected = "White")
    ),
    mainPanel(
        h3("Summary of Findings"),
        h5("Using this tool you can explore how the relationship between Republican Win Margin and Polling Accuracy varies according to race and ethnicity"),
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    # Subset the table dataset by the chosen race_eths
     table <- table %>% filter(race_eth == input$race_eth)
    # table <- subset(table,
    #                race_eth %in% input$race_eth)
    
    
    p <- ggplot(table, aes(rep_win, accuracy)) +
      geom_point(size = input$size, col = input$colour) +
      ggtitle(input$title) +
      xlab("Republic Win Margin")+ ylab("Polling Accuracy")+
      theme_light() +
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      geom_jitter()
      
    
     if (input$fit) {
       p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)