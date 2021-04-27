library(shiny)
library(tidyverse)
library(ggrepel)
library(here)
library(shinythemes)
library(datasets)
library(mdsr)
library(gapminder)

# Individual Contributions Maps for Each State
# user can choose a state and a candidate

# data wrangling
individual_contributions <- read_csv(here("Data", "IndividualContributions.csv"))

individual_contributions <- individual_contributions %>%
  pivot_longer(!State, names_to = "Candidate", values_to = "Contribution")



state_info <- data.frame(state_full = tolower(state.name) , State = state.abb,
                         Region = state.region)

usa_states <- map_data(map = "state"
                       , region = ".")

contributions_map <- individual_contributions %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("state_full" = "region"))

###################################################
# define choice values and labels for user inputs #
###################################################

# for selectizeInput choice for State, pull directly from data
state_choice <- unique(contributions_map$State)
candidate_choice <- unique(contributions_map$Candidate)


############
#    ui    #
############
ui <- navbarPage(
  
  theme = shinytheme("flatly"),
  
  title="Presidential Election Campaign Finances",
  
  tabPanel(
    title = "Individual Contributions to Candidates",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "state"
                       , label = "Choose a state to examine:"
                       , choices = state_choice
                       , selected = "Alabama"
                       , multiple = FALSE)
      ),
      mainPanel(
        plotOutput(outputId = "scatter")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  
  # INTERACTIVE SCATTERPLOT showing voting rates over time for each state
  output$map <- renderPlot({
    voting_rates %>%
      filter(state == input$state)  %>%
      ggplot(aes_string(x= "year", y="voting_rate", size = input$pt_size)) +
      geom_point(color = "#2c7fb8", size = 3) + 
      ylim(30, 85) +
      labs(x = "Year", y = "Voting Eligible Population (VEP) Turnout Rate"
           , title = "Voting Turnout over Time") +
      geom_label_repel(data = filter(voting_rates, state %in% input$id_name)
                       , aes(label = state), show.legend = FALSE) +
      scale_x_discrete(limits=c(1980,1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020))
  
  # Map
    ggplot(contributions_map, aes(x = long, y = lat, group = group,
                                  fill = bloomberg_michael_r)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(fill = "Individual Contributions for Michael Bloomberg") +
      theme(legend.position="bottom") +
      scale_fill_distiller(palette = "BuPu", direction = "horizantle")
    
    })
}



####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)