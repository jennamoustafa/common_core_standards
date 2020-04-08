# Load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

# Load data
naep_sub <- read_xlsx("naep_sub.xlsx")  %>% 
  mutate(test_type = case_when(test_type == "fourth reading" ~ "Fourth Grade Reading",
                               test_type == "eighth reading" ~ "Eighth Grade Reading",
                               test_type == "fourth math" ~ "Fourth Grade Math",
                               test_type == "eighth math" ~ "Eighth Grade Math")) %>% 
  filter(race != "NA" & jurisdiction != "National")
naep <- read_xlsx("naep.xlsx")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                
                # Title
                
                titlePanel("Common Core Implementation in the US in 2010"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput("race", inputId = "type", label = strong("Race"),
                                choices = unique(naep_sub$race),
                                selected = "Race"),
                    
                    # Select type of trend to plot
                    selectInput("test_type", inputId = "type", label = strong("Test Type"),
                                choices = unique(naep_sub$test_type),
                                selected = "Test Type"),
                    
                    # Select type of trend to plot
                    selectInput("jurisdiction", inputId = "type", label = strong("State"),
                                choices = unique(naep_sub$jurisdiction),
                                selected = "Jurisdiction"),
                  ),
                  # Output: Description, lineplot, and reference
                  mainPanel(
                            plotOutput(outputId = "plot"),
                            tags$a(href = "https://www.nationsreportcard.gov/", "Source: Nation's Report Card", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  # Subset data
  selected_subgroups <- reactive({
    naep_sub %>%
      filter(test_type == input$test_type) %>% 
      filter(race == input$race) %>% 
      filter(jurisdiction == input$jurisdiction)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$plot <- renderPlot({
    ggplot(selected_subgroups(), aes(year, score)) + 
      geom_point() +
      geom_vline(xintercept = 2010) + 
      geom_path() +
      labs(x = "Year",
           y = "Average State Test Score",
           title = "Scores Before and After \n Common Core Implementation in 2010")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)