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
                       , label = "Choose a state:"
                       , choices = state_choice
                       , selected = "Alabama"
                       , multiple = FALSE,
                       
                       inputId2 = "candidate"
                       , label2 = "Choose a candidate:"
                       , choices2 = candidate_choice
                       , selected2 = "BIDEN, JOSEPH R JR"
                       , multiple2 = FALSE)
      ),
      mainPanel(
        plotOutput(outputId = "map")
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
    contributions_map %>%
      filter(state == input$state)  %>%
      filter(candidate = input$candidate) %>%
      ggplot(contributions_map, aes(x = long, y = lat, group = group,
                                    fill = candidate_choice)) +
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