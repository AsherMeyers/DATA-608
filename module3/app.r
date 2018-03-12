library(shiny)
#runExample("01_hello")

library(shiny)
library(reshape2)
library(dplyr)

df <- read.csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/module3/mortality.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Infant Mortality!"),
  
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
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    #x    <- faithful$waiting
    x <- subset(df, Year == input$year & ICD.Chapter == input$cause)
    x <- x[order(-x$Crude.Rate),]
    
    plot1 <- barplot(x$Crude.Rate, names.arg = x$State, horiz = TRUE, las = 2,
                     col = topo.colors(50, alpha = 1), border = "white",
                     xlab = "Infant Mortality Rate, by State",
                     main = "Infant Mortality, per 100k")
    
    data = melt(x, id = "Year")
  })
}

library(dplyr)

stateName = "CA"
cause = "Neoplasms"

state <- df %>%
        filter(State == stateName, ICD.Chapter == cause) %>%
        mutate(StateTrend = 100*Crude.Rate / Crude.Rate[1]) 

national <- df %>%
  filter(ICD.Chapter == cause) %>%
  filter(State != stateName) %>%
  group_by(Year) %>%
  summarise(natDeaths = sum(Deaths), 
            natPop = sum(Population)) %>%
  mutate(natRate = 10^5 * natDeaths/natPop) %>%
  mutate(natTrend = 100 * natRate / natRate[1])

dfCompare = data.frame(Year = state$Year, State = state$StateTrend, 
                       National = national$natTrend)
melted = melt(dfCompare, id = "Year")

ggplot(data = melted, aes(x=Year, y = value, color = variable)) +                    
  geom_line() + ylab("Crude Death Rate, 100 in 1999") + 
  ggtitle(paste0("National vs ", stateName, " Infant Mortality Rates")) + 
  scale_x_continuous(breaks = seq(1999, 2010, by = 1))

#from https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph


plot(x = y$Year, y = 100*y$Crude.Rate/y$Crude.Rate[1], type = "l", 
     lty = 1, pch = "o", col = "blue", ylim = c(ymin, ymax),
     main = "Infant Mortality, CA vs USA")
lines(x = y$Year, y = avg, lty = 2, col = "red")



shinyApp(ui = ui, server = server)
'x = subset(df, Year == 2010 & ICD.Chapter == "Neoplasms")
x = x[order(-x$Crude.Rate),]
barplot(x$Crude.Rate)
barplot(x$Crude.Rate, names.arg = x$State, horiz = TRUE, las = 2)
?par

head(df)
'