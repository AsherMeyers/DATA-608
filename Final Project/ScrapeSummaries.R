
library(rvest)
library(dplyr)

setwd("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project")
df <- read.csv("cities.csv", stringsAsFactors = FALSE)
df  <- as_tibble(df)

for (i in c(414:nrow(df))) {
  # Read in the URL and the required table
  url <- df$summaryURL[i]
  summary <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="container-wrapper"]/div/table') %>%
    html_table()
  summary <- summary[[1]]

    # Fill out row i with entries from the table.
    for (k in c(1:8)){
      df[i, k+6] = summary[k, 2]
  }
}
  
write.csv(df, "cities2.csv", row.names = FALSE)


setwd("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project")
df <- read.csv("cities2.csv", stringsAsFactors = FALSE)
df  <- df[, c(1:6, 8:14)]


# Add empty fields to city-level data to house data from Census reports
df$black <- NA
df$indian <- NA
df$asian <- NA
df$islander <- NA
df$mixed <- NA
df$latino <- NA
df$white <- NA
df$blackLatInd <- NA

df$foreign <- NA
df$homeownerRate <- NA
df$nonenglish <- NA # % with non-English language spoken at home
df$hsgrad <- NA
df$bachelors <- NA
df$noninsured <- NA # % without health insurance
df$povertyRate <- NA # persons in poverty, %

df$medianHome <- NA #median value of owner occupied housing
df$medianMortgage <- NA #median housing costs for homeowners with mortgage
df$medianNonMort <- NA # median owner for those without mortgages
df$medianRent <- NA 
df$retail <- NA # retail sales per capita
df$medianIncome <- NA 
df$perCapIncome <- NA # average income per person

df$householdSize <- NA # #30 
df$commuteTime <- NA # #45, mean one way commute in time for 16+ y.o. workers
df$popdensity <- NA # population per square mile

# function for converting % strings to floats
pct_to_number<- function(x){
  x_replace_pct<-sub("%", "", x)
  x_as_numeric<-as.numeric(x_replace_pct)
}

# convert money to numbers
money_to_number<- function(x){
  as.numeric(gsub("[\\$,]", "", x))
}

# Get list of city names
citylist <- df$City
citylist <- gsub("\\s", "", citylist) # Remove spaces to build URLs
citylist <- gsub("\\.", "", citylist) # Remove periods

citylist[197] <- "lacaadaflintridge"
citylist[349] <- "RollingHillsEstates"

cityUrls = rep("", length(citylist))
for (i in 1:length(citylist)) {
  cityUrls[i] <- paste0("https://www.census.gov/quickfacts/fact/csv/",citylist[i],"citycalifornia/PST045217")
}

towns <- c(14,21,95, 106, 134, 177, 197,231,234, 237, 242, 267,303, 356)

for (i in towns) {
  cityUrls[i] <- paste0("https://www.census.gov/quickfacts/fact/csv/",citylist[i],"towncalifornia/PST045217")
}

# Cities not in Quickfacts
remove <- c(7,8,12,24,36,41, 43, 45, 4, 65, 77, 85, 86, 109, 110, 114, 131, 137,
            207, 238, 245, 265, 272, 276, 277, 307, 320, 321, 325, 326, 
            342, 353, 361)
  
cityindices <- 1:475
# Remove cities that are not available in census QuickFacts, like Alturas, CA

citiesAvailable <- cityindices[! cityindices %in% remove] 

for (i in citiesAvailable) {
  url <- cityUrls[i]
  tryCatch({
  csv <- read.csv(file=url, stringsAsFactors = FALSE)
  },
  error=function(cond){return},
  finally={
  # Parsing racial percentages
  races <- unname(sapply(csv[13:19,3], pct_to_number))/100
  races <- round(races/sum(races),3) # renormalizing percents to sum to 1
  df[i, 14:20] <- races
  df$blackLatInd[i] <- df$latino[i] + df$indian[i] + df$black[i]
  
  otherPercents <- csv[c(21, 23, 32, 33, 34, 36, 48), 3]
  df[i, 22:28] <- unname(sapply(otherPercents, pct_to_number))/100
  
  moneys <- sapply(csv[c(24, 25, 26, 27, 44, 46, 47), 3], money_to_number)
  df[i, 29:35] <- unname(sapply(moneys, pct_to_number))
  
  df[i, 36:37]<- as.numeric(csv[c(30,45),3]) # household size and commute time
  df$population[i] <- as.numeric(gsub(",","",csv[2,3]))
  df$popdensity[i] <- df$population[i]/as.numeric(csv[62, 3])
  
  })
  
}
# Convert string columns 9-13, from TransparentCA's website, to floats.
df[, 9:13] <- sapply(df[, 9:13] , money_to_number)

# Filter out cities without census data and reset row indices
df <- df[-remove, ]
rownames(df) <- seq(length=nrow(df))

# Remove irrelevant columns
df <- df[, c(1, 5, 7:39)]

write.csv(df, "citiesClean.csv", row.names = FALSE)

# df <- read.csv("cities3.csv", stringsAsFactors = FALSE)

