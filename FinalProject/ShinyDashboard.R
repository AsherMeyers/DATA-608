library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)
library(xlsx)
library(ggthemes)

# Open the dataset
setwd("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project")
df <- read.csv("payrollsEdit.csv")
df <- as_tibble(df)



# Define UI for app that draws a histogram ----
th <- theme_economist() #theme_tufte() # Set theme
scaleFUN <- function(x) sprintf("$ %3.0f", x) #Tick mark format, $K

ui <- fluidPage(
  
  # App title ----
  titlePanel("City Payrolls of California"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for Full vs Part Time Job Status ----
      selectInput(inputId = "status",
                  label = "Full Time or Part Time?",
                  choices = c("FT", "PT")),
      
      # Input: Selector for County
      selectInput(inputId = "county", 
                  label = "County", 
                  choices = levels(df$County), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      # Input: Selector for City
      selectInput(inputId = "city",
                  label = "City",
                  choices = levels(df$City),
                  selected = "Los Angeles", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      # Input: Selector for Pay Type
      selectInput(inputId = "payType",
                  label = "Pay Type",
                  choices = c("BasePay", "OvertimePay", "OtherPay",	
                              "Benefits",	"TotalPay",	"TotalPayAndBenefits"),
                  selected = "TotalPayAndBenefits")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
      )
    )
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # Create dataframe to plot
    x <- df %>%
      filter(Status == input$status, County == input$county) %>% 
      #filter(City == input$city)  %>%
      select(input$payType)
    
    plotTitle <- paste0("Income Distribution of ", input$status, " workers in ", 
                        input$county, " County") 

    ggplot(data = x, aes(x[input$payType])) + geom_histogram() + th +
           ggtitle(plotTitle) + xlab("Total Pay & Benefits") +  
           scale_x_continuous(labels=scaleFUN)
    
    
  }, height = 250, width = 600)
}

shinyApp(ui = ui, server = server)