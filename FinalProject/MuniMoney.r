library(shiny)
library(dplyr)
suppressPackageStartupMessages(library(googleVis))
library(shinythemes)
library(ggplot2)
library(tidyr)
library(markdown)
library(scales)
library(ggthemes)

setwd("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project")
cities <- read.csv("citiesClean.csv", stringsAsFactors = FALSE)
options(scipen=999)
#payrolls <- read.csv("payrollsEdit.csv", stringsAsFactors = FALSE)

# List of Counties
countyNames <- c("All Counties", sort(unique(cities$County)))
countyCodes <- c(1:length(countyNames))
names(countyCodes) <- countyNames


# City-level Data Dictionary:
cityDict <- c(
  Employees = "# Of City Employees",
  FTemployees = "# Of Full Time Employees",
  MedianPay = "Median City Pay",
  MedianPayBenes = "Median City Pay & Benefits",
  TotalComp = "Total Compensation",
  CompCostPerResident = "Compensation Cost Per City Resident",
  MedianPrivatePay = "Median Private Sector Pay",
  black = "% Black",
  indian = "% American Indian",
  asian = "% Asian",
  islander = "% Pacific Islander",
  mixed = "% Mixed Race",
  latino = "% Latino",
  white = "% White",
  blackLatInd = "% Black, Latino Or American Indian",
  foreign = "% Foreign Born",
  nonenglish = "% Non-English Speaking At Home",
  homeownerRate = "Homeownership Rate, %",
  noninsured = "% Not Medically Insured",
  hsgrad = "%  HS Graduate Or Higher",
  bachelors = "% Bachelors Degree Or Higher",
  povertyRate = "% In Poverty",
  medianIncome = "Median Household Income",
  perCapIncome = "Median Individual Income",
  householdSize = "Median Household Size",
  medianRent = "Median Rent",
  medianHome = "Median Home Value",
  medianMortgage = "Median Housing Cost, Mortgaged",
  medianNonMort = "Median Housing Cost, Unmortgaged",
  commuteTime = "Median Commute Time, 1 Way",
  retail = "Median Retail Sales",
  population = "Population",
  popdensity = "Population Density / Sq Mi"
)

percentDict <- c(
  black = "% Black",
  indian = "% American Indian",
  asian = "% Asian",
  islander = "% Pacific Islander",
  mixed = "% Mixed Race",
  latino = "% Latino",
  white = "% White",
  blackLatInd = "% Black, Latino Or American Indian",
  foreign = "% Foreign Born",
  nonenglish = "% Non-English Speaking At Home",
  homeownerRate = "Homeownership Rate, %",
  noninsured = "% Not Medically Insured",
  hsgrad = "%  HS Graduate Or Higher",
  bachelors = "% Bachelors Degree Or Higher",
  povertyRate = "% In Poverty"
)

sizeDict <- c(
  Employees = "# Of City Employees",
  FTemployees = "# Of Full Time Employees",
  MedianPay = "Median City Pay",
  MedianPayBenes = "Median City Pay & Benefits",
  TotalComp = "Total Compensation",
  CompCostPerResident = "Compensation Cost Per City Resident",
  MedianPrivatePay = "Median Private Sector Pay",
  medianIncome = "Median Household Income",
  perCapIncome = "Median Individual Income",
  householdSize = "Median Household Size",
  medianRent = "Median Rent",
  medianHome = "Median Home Value",
  medianMortgage = "Median Housing Cost, Mortgaged",
  medianNonMort = "Median Housing Cost, Unmortgaged",
  commuteTime = "Median Commute Time, 1 Way",
  retail = "Median Retail Sales",
  population = "Population",
  popdensity = "Population Density / Sq Mi"
)


ui <- shinyUI(
  navbarPage(#theme = shinytheme("flatly"), 
             "CitiMeter: Revealing Urban Metrics",
             
             tabPanel(
               title = 'Introduction',
               # includeMarkdown("c:/SQLData/608/overview.md"),
               includeMarkdown("introduction.rmd"),
               #hr(),
               helpText("Data provided by TransparentCA and the US Census")
             ), # end of introduction tab
             
             tabPanel(
               title = "Salaries",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(inputId = "county", label = h4("County"), 
                               choices = sort(unique(cities$County)), #county names 
                               selected = 1, multiple = TRUE),
                   
                   selectizeInput(inputId = "city", label = h4("City"),
                               choices = cities$City,
                               selected = 1, multiple = TRUE),
                   
                   radioButtons(inputId = "jobStatus", label = h4("Full or Part Time"),
                                choices = list("All*" = 1, 
                                               "Full Time" = 2,
                                               "Part Time" = 3), selected = 1),
                   radioButtons(inputId = "gender", label = h4("Gender"),
                                choices = list("All*" = 1, 
                                               "Female" = 2,
                                               "Male" = 3), selected = 1),
                   helpText("Data shown is from all counties *and* cities selected; 
                            specify counties for entire counties, and cities for specific cities;
                            *All also contains records that are missing job status or gender")
                 ),
                 mainPanel(
                   plotOutput("plot")
                   
                   #Table 
                 )
               )
               
             ), # end of salaries tab panel
             
             tabPanel(
               title = "Compare Cities",
               sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "crit1", label = h5("Criterion 1: X-Axis"),
                                choices = unname(cityDict),
                                selected = "Compensation Cost Per City Resident"),
                 
                 selectInput(inputId = "crit2", label = h5("Criterion 2: Y-Axis"),
                             choices = unname(cityDict),
                             selected = "% In Poverty"),
                 
                 selectInput(inputId = "colorBy", label = h5("Color Points By..."),
                             choices = unname(percentDict),
                             selected = "% Black, Latino Or American Indian"),
                 
                 selectInput(inputId = "sizeBy", label = h5("Size Markers By..."),
                             choices = unname(sizeDict),
                             selected = "Population"),
                 
                 selectizeInput(inputId = "countyFilter", label = h5("County"), 
                                choices = countyNames, 
                                selected = "All Counties", multiple = TRUE)
               ),
               mainPanel(
                 plotOutput(outputId = "cityPlot",
                            hover = "city_hover"),
                 
                 verbatimTextOutput("city_info"),
                 
                 helpText("All figures are annual;
                          Median pay figures for city staff are full-time, 
                          year-round workers only")
                 
                 )
               )
               ), # end of city comparison tab panel
             
             tabPanel(
               title = "Gender"
             ),
             tabPanel(
               title = "Tables"
              
               # Tab to display selection of employee/city data tables, with
               # option to download
             )
             
             
             
  ))





server <- function(input, output) {
  
  
  
  # City Comparison Tab
  output$cityPlot <- renderPlot({
    
    # Subset of Cities, based on selections
    x <- reactive({
      req(input$crit1, input$crit2, input$colorBy, input$sizeBy)
      
      # Obtain column names from labels selected
      crit1 = names(cityDict)[which(unname(cityDict)== input$crit1)]
      crit2 = names(cityDict)[which(unname(cityDict)== input$crit2)]
      colorBy = names(cityDict)[which(unname(cityDict)== input$colorBy)]
      sizeBy = names(cityDict)[which(unname(cityDict)== input$sizeBy)]
      
      
      if (!("All Counties" %in% input$countyFilter)) {
        df <- subset(cities, County %in% input$countyFilter)
      } else {
        df <- cities
      }
      df <- select(df, crit1, crit2, colorBy, sizeBy)
      df[complete.cases(df), ]
    })
    
    
    
    
    
    plotTitle <- paste0("X: ", input$crit1, " vs Y: ", input$crit2) 
    
    ggplot(data = x(), aes_string(x=names(x())[1], 
                                  y=names(x())[2])) + 
    geom_point(aes(colour = x()[3], size = x()[4])) + 
    scale_colour_gradient(low = "blue", high = "orange", 
                          label = percent_format()) +
    scale_x_continuous(labels = comma, trans="log10") +
    ylab(input$crit2) + xlab(input$crit1) +
    #theme_minimal() + 
    theme(axis.text.x = element_text(#face="bold", 
                                       color="black", 
                                       size=11, angle=0),
            axis.text.y = element_text(#face="bold", 
                                       color="black", 
                                       size=11, angle=0),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(size = 0.05, linetype = 'solid',
                                            colour = "#e9e9e9")
            
            ) + 
    labs(color = input$colorBy, size = input$sizeBy)
    
    #labs(aesthetic='custom text')  
    #scale_fill_continuous(guide = guide_legend(title = "V")) 
    #guides(fill=guide_legend(title="New Legend Title"))
  

    
  }, height = 250, width = 600)
  
  output$city_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("Mouse over plot to see coordinates\n")
      paste0(input$crit1,": ", round(input$city_hover$x, 2), "\n",
             input$crit2, ": ", round(input$city_hover$y, 2))
    }
    
    xy_str(input$city_hover)
    })
  
  }
  
  #output$cityTable <-  renderTable({cities})

    


shinyApp(ui, server)
