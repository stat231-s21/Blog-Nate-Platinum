library(shiny)
library(tidyverse)
library(ggrepel)
library(here)
library(shinythemes)
library(datasets)
library(mdsr)
library(gapminder)
library(dplyr)
library(ggplot2)
library(relayer)

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
         ))) %>%
  mutate(party = (case_when(
  str_detect(Candidate, "BIDEN, JOSEPH R JR") ~ "Democrat",
  str_detect(Candidate, "BLOOMBERG, MICHAEL R.") ~ "Democrat",
  str_detect(Candidate, "HARRIS, KAMALA D.") ~ "Democrat",
  str_detect(Candidate, "KLOBUCHAR, AMY J.") ~ "Democrat",
  str_detect(Candidate, "SANDERS, BERNARD") ~ "Democrat",
  str_detect(Candidate, "STEYER, TOM") ~ "Democrat",
  str_detect(Candidate, "TRUMP, DONALD J.") ~ "Republican",
  str_detect(Candidate, "WARREN, ELIZABETH") ~ "Democrat",
  str_detect(Candidate, "YANG, ANDREW MR.") ~ "Democrat"
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


####################################################
# define scale_distiller() for facetted map colors #
####################################################

scale_distiller <- function(aesthetics, palette,name,...) {
  scale_fill_distiller(
    aesthetics = aesthetics,
    palette = palette,
    name = name, ..., 
    limits = c(0,150),
    direction = "horizantle",
    guide = guide_legend(order = palette))
}

############
#    ui    #
############
ui <- navbarPage(
  
  theme = shinytheme("readable"),
  
  title="Individual Contributions to Candidates",
  
  tabPanel(
    title = "Map",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "candidate"
                       , label = "Choose candidate(s):"
                       , choices = candidate_choice
                       , selected = "Joe Biden", "Donald Trump"
                       , multiple = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "map1")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  
  # INTERACTIVE MAP
  
  
  output$map1 <- renderPlot({
    
    # subset data
    dems <- contributions_map %>%
      filter(candidate_cleaned %in% input$candidate & party=="Democrat") 
    reps <- contributions_map %>%
      filter(candidate_cleaned %in% input$candidate & party=="Republican") 
    
    # plot
    ggplot(mapping = aes(x = long, y = lat, group = group), color = "white") +
      (geom_polygon(data = dems, aes(fill1 = Contribution/1000000)) %>%
         rename_geom_aes(new_aes = c("fill" = "fill1"))) +
      (geom_polygon(data = reps, aes(fill2 = Contribution/1000000)) %>%
         rename_geom_aes(new_aes = c("fill" = "fill2"))) +
      facet_wrap(~candidate_cleaned) +
      scale_distiller("fill1", 1, "Democrats") +
      scale_distiller("fill2", 14, "Republicans") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Individual Contributions in Millions ($)
           ") +
      theme(legend.position="bottom"
            , legend.title = element_text(size = 16)
            , legend.text = element_text(size = 10)
            , strip.text = element_text(size = 16)
            , plot.title = element_text(size=20))
  })
  
}



####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)