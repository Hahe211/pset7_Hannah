
library(shiny)
library(tidyverse)
library(fs)
library(readr)
library(ggrepel)
library(readxl)
library(ggplot2)
library(plotly)


# all_congress <- read_rds("all_congress.rds")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Hannah Hess Set 7"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #selectInput("rep_adv", 
                 # "Select Race:",
                  #choices = c("White", "Asian", "Hispanic", "Other", "Black")
                  #making interactive part choose by race/ethnicity, used choices so that when opened an option is already filled in
          
              
          
    #  )
    ),
    
   # Show a plot of the generated distribution
    
mainPanel(
      plotOutput("distPlot")
    )
)
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    all_congress <- all_congress %>% 
      select(input$rep_adv)
    
    all_congress %>%
      filter(rep_adv == input$rep_adv) %>%
      
      #filter for the region someone selects in app
      
      ggplot(aes(x = rep_adv, y = accuracy)) + 
      geom_point() +
      #x axis is forecast, y is result, creating a scatter plot
      ggtitle("Forcasted vs. Actual Democratic Advantage in 2018 Midterms Elections by State") +
      #adding title
      xlab("Forecast Democratic Advantage based upon Upshot Polls") +
      #x label
      ylab("Result Democratic Advantage after Elections") +
      #y label
      #xlim(-.20, .20)+
      #set x limit so that it doesnt change when input is changed
     # ylim(-.2, .2)+
      #set y limit so that it doesnt change when input is changed
      theme_light() 
      #cool theme
     # geom_label_repel(aes(label = state), size = 3, force = 3) 
    #adds name tags for each point
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

