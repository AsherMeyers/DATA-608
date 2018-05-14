library(gender)

df = read.csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/payrollsMini.csv",
              stringsAsFactors=FALSE)
df <- tbl_df(df)
head(df)

# Get indices of names that are two letters or more;
# Names that are 0-1 characters long are not suitable for gender guessing
indices = c()

for (i in 1:nrow(df)) {
  indices[i] = nchar(df$First.Name[i])>1
}

# Initialize genders list to hold gender of each employee
genders <- numeric(sum(indices))

# Get names that are longer than one character
fnames <- df$First.Name[indices]

# Create table categorizing names by gender
genderLookup = gender(fnames, 
                 # years: 1956 = 2018 - gov retirement age of 62; 
                 #        1996 = 2018 - 22, new college graduate age
                 years = c(1956, 1996), 
                 method = "ssa",
                 countries="United States")

# Remove duplicate name entries
genderLookup <- unique(genderLookup)

# Remove names with ambiguous gender (less than 95% probability for either sex)
genderLookup <- subset(genderLookup, proportion_male <= 0.05 | proportion_male >= 0.95)

genderLookup <- genderLookup[, c("name", "gender")]
names(genderLookup) <- c("First.Name", "Gender")

genderedDF <- df

# Assign genders by using lookup table genderLookup
library(plyr)
genderedDF <- join(genderedDF, genderLookup, by = "First.Name")

# Convert Gender column to 1 if Male-assigned, 0 if Female-assigned, NA if not assigned gender
#genderedDF$isMale <- as.numeric(genderedDF$Gender == "male")

# Drop Gender and First Name columns to minimize table size
#genderedDF$Gender <- NULL
genderedDF$First.Name <- NULL

# Replace NA values with blanks
genderedDF$Gender <- replace(genderedDF$Gender, is.na(genderedDF$Gender), "")

# Round Total Pay figures to nearest $1 to save space
genderedDF$TotalPayAndBenefits <- round(genderedDF$TotalPayAndBenefits, 0)



write.csv(genderedDF, "payrollsMiniG.csv", row.names = FALSE)

# Dataframe with gender, city, county, pay data only
# Only records with job status and gender indicated
genderOnly  <- subset(genderedDF, !(Gender == "")) #& Status %in% c("", "FT", "PT"))  & !(Status == "") 
                      

# Remove superfluous T in status
genderOnly$Status <- gsub('T', '', genderOnly$Status)

genderOnly$Employee.Name <- NULL
genderOnly$Job.Title <- NULL
genderOnly$TotalPayAndBenefits <- round(genderOnly$TotalPayAndBenefits/1000,1)

write.csv(genderOnly, "genderOnly.csv", row.names = FALSE)
