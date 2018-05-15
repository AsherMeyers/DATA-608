library(shiny)
library(dplyr)
suppressPackageStartupMessages(library(googleVis))
library(shinythemes)
library(ggplot2)
library(tidyr)
library(markdown)
library(scales)
library(ggthemes)
library(gridExtra)
library(DT)
library(plotly)

setwd("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project")
cities <- read.csv("citiesCleanK.csv", stringsAsFactors = FALSE)
cities <- subset(cities, !(City %in% c("Irwindale", "Vernon")))

# Read in genderOnly file
gDF <- read.csv("genderOnly.csv", stringsAsFactors = FALSE)
gDF$Gender <- as.factor(gDF$Gender)
gCities <- c("All Cities", sort(unique(gDF$City)))
gCounties <- c("All Counties", sort(unique(gDF$County)))

# Raw Data Table
raw <- read.csv("Over10K.csv", stringsAsFactors = FALSE)

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
  MedianPayK = "Median City Pay, $K",
  MedianPayBenesK = "Median City Pay & Benefits",
  TotalCompM = "Total Compensation, $M",
  CompCostPerResidentK = "Compensation Cost Per City Resident, $K",
  MedianPrivatePayK = "Median Private Sector Pay, $K",
  black = "% Black",
  indian = "% American Indian",
  asian = "% Asian",
  islander = "% Pacific Islander",
  mixed = "% Mixed Race",
  latino = "% Latino",
  white = "% White",
  blackLatInd = "% Black, Latino Or Am. Indian",
  foreign = "% Foreign Born",
  nonenglish = "% Non-English Speaking At Home",
  homeownerRate = "Homeownership Rate, %",
  noninsured = "% Not Medically Insured",
  hsgrad = "%  HS Graduate Or Higher",
  bachelors = "% Bachelors Degree Or Higher",
  povertyRate = "% In Poverty",
  medianIncomeK = "Median Household Income, $K",
  perCapIncomeK = "Median Individual Income, $K",
  householdSize = "Median Household Size",
  medianRentK = "Median Rent, $K",
  medianHomeK = "Median Home Value, $K",
  medianMortgageK = "Median Housing Cost, Mortgaged, $K",
  medianNonMortK = "Median Housing Cost, Unmortgaged, $K",
  commuteTime = "Median Commute Time, 1 Way",
  retailK = "Median Retail Sales, $K",
  populationK = "Population, K",
  popdensityK = "Population Density, K / Sq Mi"
)

percentDict <- c(
  black = "% Black",
  indian = "% American Indian",
  asian = "% Asian",
  islander = "% Pacific Islander",
  mixed = "% Mixed Race",
  latino = "% Latino",
  white = "% White",
  blackLatInd = "% Black, Latino Or Am. Indian",
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
  MedianPay = "Median City Pay, $K",
  MedianPayBenes = "Median City Pay & Benefits, $K",
  TotalCompM = "Total Compensation, $M",
  CompCostPerResidentK = "Compensation Cost Per City Resident, $K",
  MedianPrivatePayK = "Median Private Sector Pay, $K",
  medianIncomeK = "Median Household Income, $K",
  perCapIncomeK = "Median Individual Income, $K",
  householdSize = "Median Household Size",
  medianRentK = "Median Rent, $K",
  medianHomeK = "Median Home Value, $K",
  medianMortgageK = "Median Housing Cost, Mortgaged, $K",
  medianNonMortK = "Median Housing Cost, Unmortgaged, $K",
  commuteTime = "Median Commute Time, 1 Way",
  retailK = "Median Retail Sales, $K",
  populationK = "Population, K",
  popdensityK = "Population Density, K / Sq Mi"
)

# Dictionary for Gender tab
gDict <- c(
  F = "Full Time",
  P = "Part Time"
)


ui <- shinyUI(fluidPage(
  navbarPage(#theme = shinytheme("flatly"),
             "CityMeter: Revealing Urban Metrics",

             tabPanel(
               title = 'Introduction',
               # includeMarkdown("c:/SQLData/608/overview.md"),
               includeMarkdown("introduction.rmd"),
               #hr(),
               helpText("Data provided by TransparentCA and the US Census")
             ), # end of introduction tab

             tabPanel(
               title = "Compare Cities",
               sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "crit1", label = h5("Criterion 1: X-Axis"),
                                choices = unname(cityDict),
                                selected = "Median Home Value, $K"),

                 selectInput(inputId = "crit2", label = h5("Criterion 2: Y-Axis"),
                             choices = unname(cityDict),
                             selected = "Compensation Cost Per City Resident, $K"),

                 selectInput(inputId = "colorBy", label = h5("Color Points By...(3)"),
                             choices = unname(percentDict),
                             selected = "% Black, Latino Or Am. Indian"),

                 selectInput(inputId = "sizeBy", label = h5("Size Markers By...(4)"),
                             choices = unname(sizeDict),
                             selected = "Population, K"),

                 selectizeInput(inputId = "countyFilter", label = h5("County"),
                                choices = countyNames,
                                selected = "All Counties", multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput(outputId = "comparePlot", 
                              height = "600px", width = "950px"),

                 verbatimTextOutput("city_info"),

                 helpText("All figures are annual;
                          Median pay figures for city staff are full-time,
                          year-round workers only")

                 )
               )
               ), # end of city comparison tab panel
             tabPanel(
               title = "Gender",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(inputId = "gCounty", label = h4("Counties"),
                                  choices = gCounties, #county names
                                  selected = "All Counties", multiple = TRUE),

                   selectizeInput(inputId = "gCity", label = h4("Cities"),
                                  choices = gCities,
                                  selected = "All Cities", multiple = TRUE),

                   radioButtons(inputId = "gStatus", label = h4("Full or Part Time"),
                                choices = list("Full Time",
                                               "Part Time",
                                               "All (including unlabeled records)"), 
                                selected = "Full Time"),
                   sliderInput(inputId = "gPercentile",
                               label = h4("Pay Percentile Range"),
                               min = 0, max = 100, post =  "%",
                               value = c(0,100)),
                   sliderInput(inputId = "gWorkforce",
                               label = h4("City Staff Size (both genders)"),
                               min = 1, max = 40000, sep = ",",#post =  "%",
                               value = c(50,40000))
                  ),
                 mainPanel(
                   fluidRow(
                     verticalLayout(
                       splitLayout( cellWidths = c("50%", "50%"),
                                     plotlyOutput("gBoxPlot", height = "300px"), #of individual wage earners
                                     plotlyOutput("gBarPlot", height = "300px")), # to show representation
                   splitLayout(cellWidths = c("100%", "50%"),
                               plotlyOutput("gScatterPlot", height = "350px"))) # to show representation vs wage difference
                    # of individual cities
                 )
                   )
                   ),
               helpText("Scatterplot limited to cities with 50+ employees by 
                        selected job status")
             ), # end of Gender tab panel

             tabPanel(
               title = "Tables",
                 DT::dataTableOutput("mytable")
                 
                 
               # Tab to display selection of employee/city data tables, with
               # option to download
             ) #End of Tab Panel


             ) #End of navbar page
  ) #End of Fluid Page
  ) #End of UI





server <- function(input, output) {

  #output$salaryPlot <-


  # City Comparison Tab
  output$comparePlot <- renderPlotly({

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
      df <- select(df, crit1, crit2, colorBy, sizeBy, City)
      df[complete.cases(df), ]
    })

    plotTitle <- paste0("X: ", input$crit1, " vs Y: ", input$crit2)

    ggplot(data = x(), aes_string(x=names(x())[1],
                                  y=names(x())[2])) +
    geom_point(aes(colour = x()[3], size = x()[4], hoverinfo = x()$City)) +
    scale_colour_gradient(low = "blue", high = "orange",
                          label = percent_format()) +
    scale_x_continuous(labels = comma) + #, trans="log10"
    ylab(input$crit2) + xlab(input$crit1) +
    theme(axis.text.x = element_text(#face="bold",
                                       color="black",
                                       size=9, angle=0),
            axis.text.y = element_text(#face="bold",
                                       color="black",
                                       size=9, angle=0),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(size = 0.05, linetype = 'solid',
                                            colour = "#e9e9e9")

            ) +
    labs(color = input$colorBy, size = input$sizeBy) +
    geom_smooth(method='lm', se=FALSE, color= "black",
                formula= y~x)

    #labs(aesthetic='custom text')
    #scale_fill_continuous(guide = guide_legend(title = "V"))
    #guides(fill=guide_legend(title="New Legend Title"))



  }) #, height = 450, width = 700) # end of Compare Cities plot




  #payrolls <- read.csv("payrollsEdit.csv", stringsAsFactors = FALSE)
  #c1 <- subset(payrolls, City=="Los Angeles")
  #c2 <- subset(payrolls, County=="San Francisco")
  #p <- plot_ly(alpha = 0.6) %>%
  #add_histogram(x = c1$TotalPayAndBenefits) %>%
  #add_histogram(x = c2$TotalPayAndBenefits) %>%
  #layout(barmode = "overlay")
  #p
  g <- reactive({
    # Gender inputs used
    req(input$gCity, input$gCounty, input$gStatus, input$gPercentile)

    # Obtain column names from labels selected
    gStatus = names(gDict)[which(unname(gDict)== input$gStatus)]

    # Create dataframe to display
    gg <- gDF

    # If "All Counties" is not selected, filter by county
    if (!("All Counties" %in% input$gCounty)) {
      gg <- subset(gg, County %in% input$gCounty)
    }

    # If "All Cities" is not selected, filter by city
    if (!("All Cities" %in% input$gCity)) {
      gg <- subset(gg, City %in% input$gCity)
    }

    # Subset records by pay percentile
    gg <- subset(gg, TotalPayAndBenefits >= quantile(gg$TotalPayAndBenefits, input$gPercentile[1]/100)
                 & TotalPayAndBenefits <= quantile(gg$TotalPayAndBenefits, input$gPercentile[2]/100))

    # Filter by part/full time status selected
    if (input$gStatus == "Full Time"){
      gg <- subset(gg, Status == "F")
    } else if (input$gStatus == "Part Time") {
      gg <- subset(gg, Status == "P")
    }
    
    staffSize <- gg %>%
      dplyr::count(City) %>%
      dplyr::filter(n >= input$gWorkforce[1], n<= input$gWorkforce[2])
    
    gg <- subset(gg, City %in% staffSize$City)

    gg
  }) # End of gBoxPlot Reactive definition
# Begin Gender Plots -----------------------------------------
  output$gBoxPlot <-  renderPlotly({



    # build graph with ggplot syntax
    ggplot(g(), aes(x = Gender, y = TotalPayAndBenefits)) +
      geom_boxplot(fill = c("purple", "blue"), alpha = 0.6) +
      xlab("Gender") + ylab("Total Pay and Benefits, $K") +
      ggtitle(paste0("Pay Distribution by Gender, n = ", formatC(nrow(g()), format = "d", big.mark=","))) +
      theme_bw() +
      #scale_y_continuous(breaks = 1000*c(30,75,100,150,200,300,400,500,600))
      scale_y_continuous(breaks = pretty(g()$TotalPayAndBenefits, n = 6), labels = dollar) +
      stat_summary(fun.y=mean, colour="black", geom="point",
                   shape=18, size=3) +
      theme(axis.text.x = element_text(size = 12),
            axis.title.x= element_text(size = 12),
            axis.title.y= element_text(size = 14),
            axis.text.y = element_text(size = 9),
            title = element_text(size = 12))

  }) # End rendering of gBoxPlot

    output$gBarPlot <-  renderPlotly({
      malePct = round(mean(g()$Gender == "male")*100,0)
      femalePct = 100 - malePct
      
      ggplot(g(), aes(x = Gender)) +
        geom_bar(fill = c("purple", "blue"), alpha = 0.6) +
        xlab("Gender") + ylab("Number of Employees") +
        #scale_y_continuous(labels = dollar) +
        scale_x_continuous(label = percent_format()) +
        scale_y_continuous(label = comma_format()) +
        ggtitle(paste0("Workforce: ", femalePct,"% Female, ",
                       malePct, "% Male")) +
        theme_bw() + scale_x_discrete(breaks = NULL) +
        theme(axis.text.x = element_blank(),
              axis.title.x= element_text(size = 12),
              axis.title.y= element_text(size = 14),
              axis.text.y = element_text(size = 9),
              title = element_text(size = 12))

  }) # End rendering of gBarPlot

      output$gScatterPlot <-  renderPlotly({
        medCityPay <- aggregate(g()$TotalPayAndBenefits, list(g()$City), median)

        medWageGap <- g() %>%
          dplyr::group_by(City, Gender) %>%
          dplyr::summarise(median = median(TotalPayAndBenefits)) %>%
          dplyr::filter(!("Kingsburg" %in% City | "La Puente" %in% City | 
                            "Hidden Hills" %in% City)) %>% # City with no female staff
          tidyr::spread(Gender, median) %>%
          dplyr::mutate(diff = male - female)

        # Create list of cities with 50+ employees


        # Calculate Female % by City
        cityFemalePct <- g() %>%
          dplyr::count(City, Gender) %>%
          dplyr::group_by(City) %>%
          dplyr::mutate(freq = n / sum(n)) %>%
          dplyr::filter(Gender == "female", City %in% g()$City) %>%
          dplyr::select(City, freq, n)

        ScatData <- data.frame(City = cityFemalePct$City, 
                               Frequency = round(cityFemalePct$freq,3),
                               Difference = medWageGap$diff, n = cityFemalePct$n)
        CityFemaleStaff <- paste0(ScatData$City, " ,",ScatData$n)
          ggplot(ScatData, aes(x = Frequency, y = Difference, 
                               hoverinfo = CityFemaleStaff)) +
            geom_point(alpha = 0.5, size = log10(ScatData$n)) +
            xlab("% Female Workforce") + ylab("Median Wage Delta, $K (M - F)") +
            ggtitle(paste0("Gender Representation & Wage Differences ($K): ", 
                           nrow(ScatData), " Cities")) +
            theme_bw() +
            scale_y_continuous(labels = dollar) +
            scale_x_continuous(labels = percent) +
            theme(axis.text.x = element_text(size = 12),
                  axis.title.x= element_text(size = 14),
                  axis.title.y= element_text(size = 14),
                  axis.text.y = element_text(size = 12),
                  title = element_text(size = 12))

  })  # End rendering of gScatterPlot
      
      output$mytable = DT::renderDataTable({
        raw
      })

}


shinyApp(ui, server)
