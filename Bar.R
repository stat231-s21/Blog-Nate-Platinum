# this code reproduces the histogram panel only from the electric skateboards app
library(shiny)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)



###############
# import data #
###############
contribution_breakdown <- read_csv(here("Data", "ContributionBreakdown.csv"))

contribution_breakdown_clean <- contribution_breakdown %>%
  pivot_longer(!Candidate, names_to = "Category", values_to = "Expenditure") %>%
  janitor::clean_names() %>%
  filter(candidate != "Bernie Sanders") %>%
  filter(candidate != "Elizabeth Warren") %>%
  filter(candidate != "Andrew Yang") %>%
  filter(candidate != "Amy Klobuchar") %>%
  filter(candidate != "Tom Steyer") %>%
  filter(candidate != "Michael Bloomberg") %>%
  filter(candidate != "Kamala Harris")

###################################################
# define choice values and labels for user inputs #
###################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)
# for selectInput, 'choices' object should be a NAMED LIST
category_values <- unique(contribution_breakdown_clean$category)


############
#    ui    #
############
ui <- navbarPage(
  
  title="Contribution Breakdown",
  
  tabPanel(
    title = "Bar Plot",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "barvar"
                    , label = "Choose a category"
                    , choices = category_values
                    , selected = "$200 and under"
                    , multiple = FALSE),
      ),
      mainPanel(
        plotOutput(outputId = "bar")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){

  
  output$bar <- renderPlot({
    ggplot(data = contribution_breakdown_clean, aes(x=candidate, y=expenditure/1000000) +
      geom_bar(stat="identity", fill = candidate))+
      theme_minimal()
  })
  
  }

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)