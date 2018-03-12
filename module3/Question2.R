#Mortality Rate over Time

"Often you are asked whether particular States are improving their mortality 
rates (per cause) faster than, or slower than, the national average. Create a 
visualization that lets your clients see this for themselves for one cause of 
death at the time. Keep in mind that the national average should be weighted by 
the national population."
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
  titlePanel("US Infant Mortality Trend, 1999-2010"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the Cause of death
      selectInput(inputId = "cause", 
                  label = "Cause of Death", 
                  choices = levels(df$ICD.Chapter), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      # Input: Slider for the State
      selectInput(inputId = "state", 
                  label = "State", 
                  choices = levels(df$State), 
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
    
    # Create dataframe with selected state data
    state <- df %>%
      filter(State == input$state, ICD.Chapter == input$cause) %>%
      mutate(StateTrend = 100*Crude.Rate / Crude.Rate[1]) 
    
    # Create dataframe with national data, not including selected state
    national <- df %>%
      filter(ICD.Chapter == input$cause) %>%
      filter(State != input$state) %>%
      group_by(Year) %>%
      summarise(natDeaths = sum(Deaths), 
                natPop = sum(Population)) %>%
      mutate(natRate = 10^5 * natDeaths/natPop) %>%
      mutate(natTrend = 100 * natRate / natRate[1])
    
    dfCompare = data.frame(Year = state$Year, State = state$StateTrend, 
                           National = national$natTrend)
    melted = melt(dfCompare, id = "Year")
    
    #plotTitle <- paste0("Cause of Death: ", input$cause) 
    
    ggplot(data = melted, aes(x=Year, y = value, color = variable)) +                    
      geom_line() + ylab("Crude Death Rate, 100 in 1999") + 
      theme_classic(base_size = 15) +
      ggtitle(paste0("National vs ", input$state, " Infant Mortality Rates")) + 
      scale_x_continuous(breaks = seq(1999, 2010, by = 1))
    
  }, height = 250, width = 600)
}

shinyApp(ui = ui, server = server)
