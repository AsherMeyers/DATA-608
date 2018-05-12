# -*- coding: utf-8 -*-
"""
Created on Mon Mar 19 18:54:08 2018

@author: asher
"""

import pandas as pd
import numpy as np

# Read in list of cities and their URLs 
# URLS were gleaned from copy pasting list online and creating a regular expression
# to obtain their URL
cities = pd.read_csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/cities.csv",
                     encoding = "latin1")

# 94 - Corte Madera 2016 data is missing, so the URL draws on 2015 data.
cities.iloc[94, 1] = "https://transcal.s3.amazonaws.com/public/export/corte-madera-2015.csv"

# Obtain first city, to initialize the dataframe
df = pd.read_csv(cities["url"][0])

# Concatenate the remaining city data
for i in range(0, len(cities)):
    dfi = pd.read_csv(cities["url"][i])
    df = pd.concat([df, dfi])
    print(i)
    print(cities.iloc[i,0])
    
df.to_csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/raw.csv", 
          index = False)

df = pd.read_csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/raw.csv", 
                 encoding = 'latin1')



# Remove records with zero or low pay ($1000)
#df = df[df["Total Pay & Benefits"] >= 1000]

# Replace text strings indicated missing values with nan
df.replace(to_replace="Not Provided",value=np.nan, inplace = True)
df.replace(to_replace="Aggregate",value=np.nan, inplace = True)

# Homogenize job titles
df["Job Title"] = df["Job Title"].str.strip()
df["Job Title"] = df["Job Title"].str.title()
df["Job Title"].replace(to_replace=["-", "/", "\."],
                      value=" ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=["   ", "  "],
                      value=" ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=["Sr", "sr", "Sr\.", "sr\."],value="Senior", 
                          inplace = True, regex=True)
df["Job Title"].replace(to_replace=["Asso ","Assoc ",],
                      value="Associate ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=["Asst"],
                      value="Assistant ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" 1",
                      value=" I", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" 2",
                      value=" Ii", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" 3",
                      value=" Iii", inplace = True, regex=True)
df["Job Title"].replace(to_replace=["Offier", "Ofcr"],
                      value="Officer", inplace = True, regex=True)
df["Job Title"].replace(to_replace="ire Fighter",
                      value="irefighter", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Wrkr",
                      value=" Worker", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Maint ",
                      value=" Maintenance ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Eng Civil",
                      value=" Civil engineer", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Mgmt",
                      value=" Management", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Sys ",
                      value=" System ", inplace = True, regex=True)
df["Job Title"].replace(to_replace="Pol ",
                      value="Police ", inplace = True, regex=True)
df["Job Title"].replace(to_replace="Equip ",
                      value="Equipment ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Engrg ",
                      value=" Engineering ", inplace = True, regex=True)
df["Job Title"].replace(to_replace=" Enfrc ",
                      value=" Enforcement ", inplace = True, regex=True)

#sum(df[df["Status"] == "FT"]["Job Title"].value_counts()[0:200])

#topjobs = df[df["Status"] == "FT"]["Job Title"].value_counts()

#topjobs.to_csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/topjobs.csv", 
#          index = True)

# Fix employee names
df["Employee Name"].replace(to_replace=["-", "/", "\."],
                      value=" ", inplace = True, regex=True)

McClendonC = df[df["Employee Name"] == "McClendon C"].index
df.loc[McClendonC, "Employee Name"] = "McClendon, C"

df["Employee Name"] = df["Employee Name"].str.title()



# Change any name containing redacted  or deleted to blank
df.loc[df['Employee Name'].str.contains(['edacted', 'edactd', 'zzz'], na=False), 'Employee Name'] = ''

# Remove Jrs and Srs, etc
df["Employee Name"].replace(to_replace=[", Jr.",", Jr", ", JR", " Jr", "Jr ",
                                           " Ii", "Ii ", " II", "II ", 
                                           ", III", ", Iii", " Iii", "Iii ",
                                        ", Iv", ", Sr", ", Sr."],
                              value="", inplace = True, regex=True)

# Remove leading and trailing commas
df["Employee Name"] = df["Employee Name"].str.strip(',')

# Remove double, triple, quadruple spaces
df["Employee Name"].replace(to_replace=["    ","   ","  "],
                              value=" ", inplace = True, regex=True)

# Remove erroneous comma
df["Employee Name"].replace(to_replace=[",-"],
                              value="-", inplace = True, regex=True)

# Create first name column
df["First Name"] = len(df)*[""]

# Get first names of people with commas in their name

# Names & indexes of people with commas in their name


comma_names = df[df["Employee Name"].str.contains(',', na=False)].iloc[:,0]
comma_names = comma_names.reset_index(drop=True)
comma_indexes = df[df["Employee Name"].str.contains(',', na=False)].index

# Split on comma, for names with commas
full_names = comma_names.str.split(', ')

# list to hold first names
firsts = pd.Series(len(full_names) *[None] )

# Loop to get everything after the comma
for i in range(len(full_names)): 
    try: 
        firsts[i] = full_names[i][1]
        #removed below, since genderizing will avoid one letter names
        #if len(firsts[i]) == 1:
        #    firsts[i] = ""
    except:
        firsts[i] = ""

# Split on space each name into a list of two or more names, 
# and take the first element
firsts = firsts.str.split(' ')
firsts = pd.Series([item[0] for item in firsts])
firsts = firsts.str.title()

firsts.replace(to_replace=["\."],
                              value="", inplace = True, regex=True)

# Re-insert names at their original indexes
df.loc[comma_indexes, "First Name"] = firsts

# Names & indexes of people without commas in their name
noncomma_names = df[~df["Employee Name"].str.contains(',', na=True)].iloc[:,0]
noncomma_names = noncomma_names.reset_index(drop=True)
noncomma_indexes = df[~df["Employee Name"].str.contains(',', na=True)].index

#testt = df.iloc[[262,263,1175],:].copy()
#testt[~testt["Employee Name"].str.contains(',', na= True)].iloc[:,0]

# list to hold first names
firsts = pd.Series(len(noncomma_names) *[None] )
firsts = noncomma_names.str.split(' ')

#i=0
for i in range(len(firsts)):
    if len(firsts[i]) == 1:
        firsts[i] = ""
    else:
        firsts[i]=firsts[i][0]
    

df["First Name"][noncomma_indexes] = firsts
#df.loc[list(noncomma_indexes), "First Name"] = firsts

# Clean first names
df["First Name"] = df["First Name"].str.title()
df["First Name"].replace(to_replace=["\."],
                              value=" ", inplace = True, regex=True)
df["First Name"].replace(to_replace=["    ", "   ", "  "],
                              value=" ", inplace = True, regex=True)

# Create county column
df = df.rename(index=str, columns = {"Agency":"City"})
df = df.merge(cities.loc[:, ["City", "County"]], on="City", how="left")


df.to_csv("C:/Users/asher/Documents/GitHub/CUNY-DATA-608/Final Project/payrolls.csv", 
          index = False)

# Get city stats

from urllib.request import urlopen
import urllib
from bs4 import BeautifulSoup
import requests

url = "https://transparentcalifornia.com/salaries/2016/adelanto/summary/"
req = urllib.request.Request(url, headers={'User-Agent' : "Magic Browser"}) 
con = urllib.request.urlopen( req )

req = requests.get(url)
soup = BeautifulSoup(con)
table = soup.findAll("table record-detail summary")
table

print(con.read())

soup = BeautifulSoup(con.read())
soup.find_all('table')




city_page = urlopen(url)
soup = BeautifulSoup(stock_page, 'lxml')
price = float(soup.find('span', {'class' :"Trsdu(0.3s) Fw(b) Fz(36px) Mb(-4px) D(ib)"}).text)


