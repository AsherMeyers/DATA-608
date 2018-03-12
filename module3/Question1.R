#runExample("01_hello")

library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)


raw <- read.csv("https://raw.githubusercontent.com/AsherMeyers/DATA-608/master/module3/mortality.csv")
df <- subset(raw, raw$ICD.Chapter != unique(raw$ICD.Chapter)[18])
df$ICD.Chapter <- factor(df$ICD.Chapter) #Refactor to remove Codes for special purposes level 

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("US Infant Mortality, by State, 1999-2010"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the year ----
      sliderInput(inputId = "year",
                  label = "Year",
                  min = min(df$Year),
                  max = max(df$Year),
                  value = 2010,
                  step = 1
                  #value = 30
      ),
      
      # Input: Slider for the Cause of death
      selectInput(inputId = "cause", 
                  label = "Cause of Death", 
                  choices = levels(df$ICD.Chapter), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
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
      filter(Year == input$year, ICD.Chapter == input$cause) %>%
      select(State, Crude.Rate)
      
    plotTitle <- paste0("Cause of Death: ", input$cause) 
    
    ggplot(data = x, aes(x=reorder(State, -Crude.Rate), y=Crude.Rate, fill=-Crude.Rate)) + 
      geom_bar(stat="identity") +  
      ggtitle(plotTitle) + xlab("State") + 
      ylab("Rate, per 100,000 born") + labs(fill = "Rate") + theme_classic() +
      theme(axis.text.x = element_text(size=10)) + coord_fixed(ratio = 3) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      coord_cartesian(ylim = c(0, max(x$Crude.Rate)))
    
  
  }, height = 250, width = 600)
}

shinyApp(ui = ui, server = server)
