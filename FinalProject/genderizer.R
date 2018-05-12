library(gender)

df = read.csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/payrolls.csv",
              stringsAsFactors=FALSE)

head(df)
rownum = dim(df)[1]

# Initialize genders list to hold gender of each employee
genders = numeric(rownum)

# Set gender to "" [blank] where the first name is 0 or 1 letter
genders[which(nchar(df$First.Name)<2)]=""

# Dataframe with first names and their original indexes
fnameDF = data.frame(Index = which(nchar(df$First.Name)>1),
           firstName = df$First.Name[which(nchar(df$First.Name)>1)],
           stringsAsFactors=FALSE)

# Alphabetize by first name
fnameDF = fnameDF[order(fnameDF$firstName), ]

fnameDF$Gender = 
  
genders = gender(fnameDF$firstName, 
                               years = c(1932, 2000),
                               method = "ssa",
                               countries="United States")$gender

gender_tbl = gender(firstNames, 
                    years = c(1932, 2000),
                    method = "ssa",
                    countries="United States")


for (i in c(1: rownum)) {
  name = df$First.Name[i]
  if (nchar(name) < 2) {
    next
  } 
  genders[i] <- gender(name)$gender #
}
df$Gender = genders
View(df)

df$Gender

genders = gender(df$First.Name[c(1:150)])$gender
df$First.Name 

firsts = subset(df$First.Name, nchar(df$First.Name)>1 )

