library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Common Core Implementation in the CA in 2010"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("myImage1"),
    plotOutput("myImage2")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$myImage1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    filename <- normalizePath(file.path(paste('plot1', '.png', sep='')))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)

  output$myImage2 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    filename <- normalizePath(file.path(paste('plot2', '.png', sep='')))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
