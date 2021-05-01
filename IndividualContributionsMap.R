library(shiny)
library(tidyverse)
library(ggrepel)
library(here)
library(shinythemes)
library(datasets)
library(mdsr)
library(gapminder)
library(dplyr)

# Individual Contributions Maps for Each State
# user can choose candidate(s)

# data wrangling
individual_contributions <- read_csv(here("Data", "IndividualContributions.csv"))

# filtering out contributions from Armed Forces and other U.S. territories etc. that are listed as states
individual_contributions <- individual_contributions %>%
  pivot_longer(!State, names_to = "Candidate", values_to = "Contribution") %>%
  filter(State != "ZZ") %>%
  filter(State != "AA") %>%
  filter(State != "AE")  %>%
  filter(State != "AP")  %>%
  filter(State != "AS")  %>%
  filter(State != "GU")  %>%
  filter(State != "MP")  %>%
  filter(State != "OT")  %>%
  filter(State != "PR")  %>%
  mutate(candidate_cleaned = (case_when(
         str_detect(Candidate, "BIDEN, JOSEPH R JR") ~ "Joe Biden",
         str_detect(Candidate, "BLOOMBERG, MICHAEL R.") ~ "Michael Bloomberg",
         str_detect(Candidate, "HARRIS, KAMALA D.") ~ "Kamala Harris",
         str_detect(Candidate, "KLOBUCHAR, AMY J.") ~ "Amy Klobuchar",
         str_detect(Candidate, "SANDERS, BERNARD") ~ "Bernie Sanders",
         str_detect(Candidate, "STEYER, TOM") ~ "Tom Steyer",
         str_detect(Candidate, "TRUMP, DONALD J.") ~ "Donald Trump",
         str_detect(Candidate, "WARREN, ELIZABETH") ~ "Elizabeth Warren",
         str_detect(Candidate, "YANG, ANDREW MR.") ~ "Andrew Yang"
         )))
  

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
candidate_choice <- unique(contributions_map$candidate_cleaned)[-10]


############
#    ui    #
############
ui <- navbarPage(
  
  theme = shinytheme("flatly"),
  
  title="Individual Contributions to Candidates",
  
  tabPanel(
    title = "Map",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "candidate"
                       , label = "Choose candidate(s):"
                       , choices = candidate_choice
                       , selected = "Joe Biden"
                       , multiple = TRUE)
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
  
  # INTERACTIVE MAP
  output$map <- renderPlot({
    contributions_map %>%
      filter(candidate_cleaned %in% input$candidate) %>%
      ggplot(aes(x = long, y = lat, group = group,
                                    fill = Contribution)) +
      geom_polygon(color = "white") +
      facet_wrap(~candidate_cleaned) +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(fill = "Individual Contributions for Chosen Candidates") +
      theme(legend.position="bottom") +
      scale_fill_distiller(palette = "BuPu", direction = "horizantle")
    })
}



####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)