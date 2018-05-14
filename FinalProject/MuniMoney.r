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

setwd("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project")
cities <- read.csv("citiesClean.csv", stringsAsFactors = FALSE)
cities <- subset(cities, !(City %in% c("Irwindale", "Vernon")))

# Read in genderOnly file
gDF <- read.csv("genderOnly.csv", stringsAsFactors = FALSE)
gDF$Gender <- as.factor(gDF$Gender)
gCities <- c("All Cities", sort(unique(gDF$City)))
gCounties <- c("All Counties", sort(unique(gDF$County)))

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
                   plotOutput("salaryPlot")

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
                                selected = "% White"),

                 selectInput(inputId = "crit2", label = h5("Criterion 2: Y-Axis"),
                             choices = unname(cityDict),
                             selected = "Compensation Cost Per City Resident"),

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
                 plotOutput(outputId = "comparePlot"),

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
                                               "All (including unlabeled records)"), selected = "Full Time"),
                   sliderInput(inputId = "gPercentile",
                               label = h4("Pay Percentile Range"),
                               min = 0, max = 100, post =  "%",
                               value = c(0,100))
                  ),
                 mainPanel(
                   fluidRow(
                     verticalLayout(
                       splitLayout( cellWidths = c("50%", "50%"),
                                     plotOutput("gBoxPlot"), #of individual wage earners
                                     plotOutput("gBarPlot")), # to show representation
                   splitLayout(cellWidths = c("100%", "50%"),
                               plotOutput("gScatterPlot"))) # to show representation vs wage difference
                    # of individual cities
                 )
                   )
                   )

             ), # end of salaries tab panel

             tabPanel(
               title = "Tables"

               # Tab to display selection of employee/city data tables, with
               # option to download
             ) #End of Tab Panel


             ) #End of navbar page
  ) #End of Fluid Page
  ) #End of UI





server <- function(input, output) {

  #output$salaryPlot <-


  # City Comparison Tab
  output$comparePlot <- renderPlot({

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
    scale_colour_gradient(low = "palegreen", high = "brown",
                          label = percent_format()) +
    scale_x_continuous(labels = comma) + #, trans="log10"
    ylab(input$crit2) + xlab(input$crit1) +
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
    labs(color = input$colorBy, size = input$sizeBy) +
    geom_smooth(method='lm', se=FALSE, color= "black",
                formula= y~x)

    #labs(aesthetic='custom text')
    #scale_fill_continuous(guide = guide_legend(title = "V"))
    #guides(fill=guide_legend(title="New Legend Title"))



  }, height = 450, width = 700) # end of Compare Cities plot

  output$city_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("\n\n\nMouse over plot to see coordinates\n")
      paste0("\n\n\n",input$crit1,": ", round(input$city_hover$x, 2), "\n",
             input$crit2, ": ", round(input$city_hover$y, 2))
    }

    xy_str(input$city_hover)
    })


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

    gg <- subset(gg, TotalPayAndBenefits >= quantile(gg$TotalPayAndBenefits, input$gPercentile[1]/100)
                 & TotalPayAndBenefits <= quantile(gg$TotalPayAndBenefits, input$gPercentile[2]/100))

    # Filter by part/full time status selected
    if (input$gStatus == "Full Time"){
      gg <- subset(gg, Status == "F")
    } else if (input$gStatus == "Part Time") {
      gg <- subset(gg, Status == "P")
    }

    gg
  }) # End of gBoxPlot Reactive definition
# Begin Gender Plots -----------------------------------------
  output$gBoxPlot <-  renderPlot({



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
            axis.title.x= element_text(size = 14),
            axis.title.y= element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))

  }) # End rendering of gBoxPlot

    output$gBarPlot <-  renderPlot({
      malePct = round(mean(g()$Gender == "male")*100,0)
      femalePct = 100 - malePct
      
      ggplot(g(), aes(x = Gender)) +
        geom_bar(fill = c("purple", "blue"), alpha = 0.6) +
        xlab("Gender") + ylab("Number of Employees") +
        #scale_y_continuous(labels = dollar) +
        scale_x_continuous(label = percent_format()) +
        scale_y_continuous(label = comma_format()) +
        ggtitle(paste0(femalePct,"% female, ",
                       malePct, "% male")) +
        theme_bw() + scale_x_discrete(breaks = NULL) +
        theme(axis.text.x = element_text(size = 12),
              axis.title.x= element_text(size = 14),
              axis.title.y= element_text(size = 14),
              axis.text.y = element_text(size = 12),
              title = element_text(size = 16))

  }) # End rendering of gBarPlot

      output$gScatterPlot <-  renderPlot({
        city50 <- g() %>%
          dplyr::count(City) %>%
          dplyr::filter(n >= 50) %>%
          dplyr::select(City)

        city50 <- subset(g(), City %in% city50$City)

        medCityPay <- aggregate(city50[, 1], list(city50$City), median)

        medWageGap <- city50 %>%
          dplyr::group_by(City, Gender) %>%
          dplyr::summarise(median = median(TotalPayAndBenefits)) %>%
          tidyr::spread(Gender, median) %>%
          dplyr::mutate(diff = male - female)

        # Create list of cities with 50+ employees


        # Calculate Female % by City
        cityFemalePct <- city50 %>%
          dplyr::count(City, Gender) %>%
          dplyr::group_by(City) %>%
          dplyr::mutate(freq = n / sum(n)) %>%
          dplyr::filter(Gender == "female", City %in% city50$City) %>%
          dplyr::select(City, freq)

        ScatData <- data.frame(City = cityFemalePct$City, freq = cityFemalePct$freq,
                               diff = medWageGap$diff)

          ggplot(ScatData, aes(x = freq, y = diff)) +
            geom_point(alpha = 0.6) +
            xlab("% Female") + ylab("Median Wage Difference, $K (M - F)") +
            ggtitle(paste0("Gender Representation & Wage Differences ($K): ", 
                           nrow(ScatData), " Cities")) +
            theme_bw() +
            scale_y_continuous(labels = dollar) +
            scale_x_continuous(labels = percent) +
            theme(axis.text.x = element_text(size = 12),
                  axis.title.x= element_text(size = 14),
                  axis.title.y= element_text(size = 14),
                  axis.text.y = element_text(size = 12),
                  title = element_text(size = 16))

  })  # End rendering of gScatterPlot

}


shinyApp(ui, server)
