# Load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(gt)
library(broom)

# Load data
naep_sub <- read_xlsx("naep_sub.xlsx") %>% 
  filter(race != "NA" & race != "American Indian/Alaska Native"
         & jurisdiction != "National" & jurisdiction != "DoDEA" & 
           jurisdiction != "District of Columbia")

no_cc <- naep_sub %>%
  filter(!is.na(cc_implementation))

naep <- read_xlsx("naep.xlsx") %>%
  filter(jurisdiction != "National" & jurisdiction != "DoDEA" &
           jurisdiction != "District of Columbia") %>% 
  mutate(test_type = case_when(test_type == "fourth reading" ~ "Fourth Grade Reading",
                        test_type == "eighth reading" ~ "Eighth Grade Reading",
                        test_type == "fourth math" ~ "Fourth Grade Math",
                        test_type == "eighth math" ~ "Eighth Grade Math"))

naep_no_cc <- naep %>% 
  filter(!is.na(cc_implementation))

# Define UI for application that draws a histogram
ui <- navbarPage("Common Core Implementation in the US",
                 
                 tabPanel("About",
                          h2("Background"), 
                          "The goal of this project is to determine how implementation of the Common Core
Standards (CSS) in the early 2010s affected achievement relative across states
relative to states that did not implement the CSS. The goal of the curriculum
standardization is to narrow achievement gaps; however, there is debate over
whether the Common Core actually achieved this goal as well as concern over
states simply adjusting their state standardized tests to allow for teachers' 
teaching to the test. To avoid this bias, I am using National Assessment of 
Educational Progress (NAEP) data, which includes average student test scores by
states on national standardized exams.",
                          h2("Data"), 
                          "The data from this project is from http://www.corestandards.org and 
nationsreportcard.gov, which respectively have data on dates regarding CSS
adoption and implementation across states and data on test scores by state and
year for fourth and eighth grade NAEP scores as well as by subcategory (e.g.
race, gender, school lunch program eligibility). School lunch eligibility is 
used as a proxy for low-income, since income levels are not reported/provided.",        
                          h2("Project Plan"),
                          "This project examines NAEP score trends across states, by race, gender, and 
income in order to determine how implementation of the CSS impacted
standardized exam scores for students overall and how the program impacted
achievement gaps. The project conducts an event-study design regressing
each average test score by state, subject, and grade against the nine
states that chose not to implement the CSS as controls."
                 ),
                 # Application title
                 tabPanel("State Comparison against Non-CCS States",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          # Select type of trend to plot
                                          selectInput(inputId = "test_type1", label = strong("Test Type"),
                                                      choices = unique(naep_sub$test_type),
                                                      selected = "Test Type"),
                                          selectInput(inputId = "jurisdiction1", label = strong("State"),
                                                      choices = unique(naep_no_cc$jurisdiction),
                                                      selected = "State")
                                        ),
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("p1"),
                                          tags$a(href = "https://www.nationsreportcard.gov/", "Source: Nation's Report Card", target = "_blank")
                                        )
                          )
                 ),
                 
                 tabPanel("Test Trends by State and Race",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          # Select type of trend to plot
                                          selectInput(inputId = "race2", label = strong("Race"),
                                                      choices = unique(naep_sub$race),
                                                      selected = "Race"),
                                          # Select type of trend to plot
                                          selectInput(inputId = "jurisdiction2", label = strong("State"),
                                                      choices = unique(naep_sub$jurisdiction),
                                                      selected = "State")
                                        ),
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("p2"),
                                          tags$a(href = "https://www.nationsreportcard.gov/", "Source: Nation's Report Card", target = "_blank")
                                        )
                          )
                 ),
                 tabPanel("Outcomes for Non-White Students by State and Test",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          # Select type of trend to plot
                                          selectInput(inputId = "jurisdiction3", label = strong("State"),
                                                      choices = unique(no_cc$jurisdiction),
                                                      selected = "Race"),
                                          # Select type of trend to plot
                                          selectInput(inputId = "test_type3", label = strong("Test Type"),
                                                      choices = unique(no_cc$test_type),
                                                      selected = "Test Type")
                                        ),
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          gt_output("t3"),
                                          plotOutput("p3"),
                                          tags$a(href = "https://www.nationsreportcard.gov/", "Source: Nation's Report Card", target = "_blank")
                                        )
                          )
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Plot 1
  # Subset data
  selected_state1 <- reactive({
    naep %>% 
      filter(jurisdiction == input$jurisdiction1) %>% 
      filter(year >= 2005)
  })
  
  selected_test1 <- reactive({
    naep %>% 
      filter(test_type == input$test_type1) %>% 
      head(1) %>% 
      pull(test_type)
  })
  
  cc_year1 <- reactive({
    naep %>% 
      filter(jurisdiction == input$jurisdiction1) %>% 
      head(1) %>% 
      pull(cc_implementation)
  }) 
  
  output$p1 <- renderPlot({
    naep %>% 
      filter(is.na(cc_implementation)) %>% 
      group_by(year, test_type) %>% 
      filter(year >= 2005) %>% 
      summarize(score = mean(score)) %>% 
      mutate(jurisdiction = "Average in States without CSS") %>% 
      merge(., selected_state1(),
            by = c("year", "jurisdiction", "score", "test_type"), all = T) %>% 
      select(year, jurisdiction, score, test_type) %>%
      ggplot(., aes(year, score, color = jurisdiction)) + 
      geom_line() +
      geom_vline(xintercept = cc_year1()) +
      facet_wrap(~test_type) +
      theme_classic() +
      geom_path() +
      labs(x = "Year",
           y = "Average Test Score",
           title = "Scores Before and After \n Common Core Implementation",
           color = "State")
  })
  
  ## Plot 2
  
  selected_subgroups2 <- reactive({
    naep_sub %>%
      filter(race == input$race2) %>% 
      filter(jurisdiction == input$jurisdiction2)
  })
  
  cc_year2 <- reactive({
    naep_sub %>% 
      filter(jurisdiction == input$jurisdiction2) %>% 
      head(1) %>% 
      pull(cc_implementation)
  }) 
  
  output$p2 <- renderPlot({
    selected_subgroups2() %>% 
      filter(jurisdiction == input$jurisdiction2,
             race == input$race2) %>%
      ggplot(., aes(year, score, color = test_type)) + 
      geom_point() +
      geom_vline(xintercept = cc_year2()) + 
      geom_path() +
      labs(x = "Year",
           y = "Average Test Score",
           title = "State Scores Before and After \n Common Core Implementation",
           color = "Grade and Subject") +
      theme_classic()
  })
  
  ## Plot 3
  
  selected_subgroups3 <- reactive({
    no_cc %>%
      filter(jurisdiction == input$jurisdiction3) %>% 
      filter(test_type == input$test_type3)
  })

  
  cc_year3 <- reactive({
    no_cc %>% 
      filter(jurisdiction == input$jurisdiction3) %>% 
      head(1) %>% 
      pull(cc_implementation)
  }) 
  
  
  output$t3 <- render_gt({
    selected_subgroups3() %>%
      filter(year >= 2005) %>% 
      filter(!is.na(race)) %>%
      group_by(jurisdiction) %>% 
      nest() %>% 
      mutate(mod = map(data, ~lm(score ~ non_white + treat_year +
                                   non_white:treat_year, data = .))) %>% 
      mutate(reg_results = map(mod, ~tidy(., conf.int = TRUE))) %>% 
      unnest(reg_results) %>% 
      ungroup() %>% 
      select(term, estimate, p.value, conf.low, conf.high) %>% 
      mutate(term = case_when(term == "(Intercept)" ~ "Intercept",
                              term == "non_white" ~ "Not White",
                              term == "treat_year" ~ "Treated Year",
                              term == "non_white:treat_year" ~ "Interaction of Race and Treated Year"),
             estimate = round(estimate, digits = 2),
             p.value = round(p.value, digits = 2),
             conf.low = round(conf.low, digits = 2),
             conf.high = round(conf.high, digits = 2)) %>% 
      gt(auto_align = TRUE) %>% 
      tab_header(title = "Effect of Common Core on Scores for Non-White Students") %>%
      cols_label(term = "Term",
                 estimate = "Estimate",
                 p.value = "p-Value",
                 conf.low = "Lower bound",
                 conf.high = "Upper bound")
  })
  
  
  output$p3 <- renderPlot({
    selected_subgroups3() %>%
      filter(!is.na(race)) %>%
      group_by(year) %>%
      nest() %>%
      filter(year >= 2005) %>% 
      mutate(mod = map(data, ~lm(score ~ non_white + treat_year, data = .)),
             reg_results = map(mod, ~ tidy(., conf.int = TRUE)),
             nw_estimate = map_dbl(reg_results,  ~ filter(., term == "non_white") %>%
                                     pull(estimate)),
             nw_low = map_dbl(reg_results,  ~ filter(., term == "non_white") %>%
                                pull(conf.low)),
             nw_high = map_dbl(reg_results,  ~ filter(., term == "non_white") %>%
                                 pull(conf.high))) %>% 
      ggplot(., aes(x = year, y = nw_estimate, ymin = nw_low, ymax = nw_high,
                    color = "blue")) +
      geom_point() +
      geom_errorbar() +
      geom_vline(xintercept = cc_year3(), linetype="dashed", color = "black") + 
      scale_color_manual(values = "skyblue") +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Year",
           y = "Coefficient",
           title = "Effect of Common Core on Racial Minority Scores over Time")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
