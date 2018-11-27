# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mixture Model Simulation"),
  
  # Sidebar with a slider input for the number of components
  sidebarLayout(
    sidebarPanel(
      sliderInput("components","Number of components:", min = 2,max = 20,value = 4),
      selectInput("N","Sample Size", choices=c(200,500,1000,2000,5000)),
      actionButton("rand",label="Sample Mu and Dev")
    ),
      
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Mixture"),
      tableOutput("Table")
    )
  )
))

