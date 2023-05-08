##5.5 Panel Creation version notes
#For air pollution creation code see Version 4.5
#This code creates panel data and saves it to avoid clutter
#check previous versions for additional notes



library(plm)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(geojsonio)
library(lmtest)
library(readxl)



#load pollution data 
#pm2.5
pm2.5_2010 <- read.csv("Structured Data/avg_pm2.5_2010.csv")
colnames(pm2.5_2010) <- c('id', 'pm2.5')
pm2.5_2015 <- read.csv("Structured Data/avg_pm2.5_2015.csv")
colnames(pm2.5_2015) <- c('id', 'pm2.5')
pm2.5_2017 <- read.csv("Structured Data/avg_pm2.5_2017.csv")
colnames(pm2.5_2017) <- c('id', 'pm2.5')
pm2.5_2019 <- read.csv("Structured Data/avg_pm2.5_2019.csv")
colnames(pm2.5_2019) <- c('id', 'pm2.5')

#no2
no2_2010 <- read.csv("Structured Data/avg_no2_2010.csv")
colnames(no2_2010) <- c('id', 'no2')
no2_2015 <- read.csv("Structured Data/avg_no2_2015.csv")
colnames(no2_2015) <- c('id', 'no2')
no2_2017 <- read.csv("Structured Data/avg_no2_2017.csv")
colnames(no2_2017) <- c('id', 'no2')
no2_2019 <- read.csv("Structured Data/avg_no2_2019.csv")
colnames(no2_2019) <- c('id', 'no2')


#pm10
pm10_2010 <- read.csv("Structured Data/avg_pm10_2010.csv")
colnames(pm10_2010) <- c('id', 'pm10')
pm10_2015 <- read.csv("Structured Data/avg_pm10_2015.csv")
colnames(pm10_2015) <- c('id', 'pm10')
pm10_2017 <- read.csv("Structured Data/avg_pm10_2017.csv")
colnames(pm10_2017) <- c('id', 'pm10')
pm10_2019 <- read.csv("Structured Data/avg_pm10_2019.csv")
colnames(pm10_2019) <- c('id', 'pm10')

#so2
so2_2010 <- read.csv("Structured Data/avg_so2_2010.csv")
colnames(so2_2010) <- c('id', 'so2')
so2_2015 <- read.csv("Structured Data/avg_so2_2015.csv")
colnames(so2_2015) <- c('id', 'so2')
so2_2017 <- read.csv("Structured Data/avg_so2_2017.csv")
colnames(so2_2017) <- c('id', 'so2')
so2_2019 <- read.csv("Structured Data/avg_so2_2019.csv")
colnames(so2_2019) <- c('id', 'so2')


#oz
oz_2010 <- read.csv("Structured Data/avg_oz_2010.csv")
colnames(oz_2010) <- c('id', 'oz')
oz_2015 <- read.csv("Structured Data/avg_oz_2015.csv")
colnames(oz_2015) <- c('id', 'oz')
oz_2017 <- read.csv("Structured Data/avg_oz_2017.csv")
colnames(oz_2017) <- c('id', 'oz')
oz_2019 <- read.csv("Structured Data/avg_oz_2019.csv")
colnames(oz_2019) <- c('id', 'oz')

#load voting data
#2019 voting

Green2019 <- read.csv("Structured Data/greenVotes2019.csv")

#remove irrelevant geographical information and electorate number 
Green2019 <- Green2019[, -c(3,4,5,6)]

#remove commas
Green2019$Green.Votes_2019 <- gsub(",", "", Green2019$Green.Votes_2019)
Green2019$Total.votes_2019 <- gsub(",", "", Green2019$Total.votes_2019)

#replace empty cells with 0
Green2019$Green.Votes_2019 <- ifelse(Green2019$Green.Votes_2019 == "", 0, Green2019$Green.Votes_2019)

#Writing green vote numbers and total vote numbers as numeric
Green2019$Green.Votes_2019 <- as.numeric(Green2019$Green.Votes_2019)
Green2019$Total.votes_2019 <- as.numeric(Green2019$Total.votes_2019)

#recalculating the vote share since they are rounded in original
Green2019$VoteShare_2019 <- Green2019$Green.Votes_2019/Green2019$Total.votes_2019*100

#compare with the percentages in the document
#they match when rounded
#remove everything except for constituency code, name, year and new calculated vote share
Green2019 <- Green2019[, -c(3,4,5)]
#2019 voting end

#2017 voting
Green2017 <- read.csv("Structured Data/greenVotes2017.csv")

Green2017 <- Green2017[, -c(3,4,5,6)]
Green2017$Green.Votes_2017 <- gsub(",", "", Green2017$Green.Votes_2017)
Green2017$Total.votes_2017 <- gsub(",", "", Green2017$Total.votes_2017)
Green2017$Green.Votes_2017 <- ifelse(Green2017$Green.Votes_2017 == "", 0, Green2017$Green.Votes_2017)
Green2017$Green.Votes_2017 <- as.numeric(Green2017$Green.Votes_2017)
Green2017$Total.votes_2017 <- as.numeric(Green2017$Total.votes_2017)
Green2017$VoteShare_2017 <- Green2017$Green.Votes_2017/Green2017$Total.votes_2017*100
Green2017 <- Green2017[, -c(3,4,5)]
#2017 voting end

#2015 voting
Green2015 <- read.csv("Structured Data/greenVotes2015.csv")


Green2015 <- Green2015[, -c(3,4,5,6)]
Green2015$Green.Votes_2015 <- gsub(",", "", Green2015$Green.Votes_2015)
Green2015$Total.votes_2015 <- gsub(",", "", Green2015$Total.votes_2015)
Green2015$Green.Votes_2015 <- ifelse(Green2015$Green.Votes_2015 == "", 0, Green2015$Green.Votes_2015)
Green2015$Green.Votes_2015 <- as.numeric(Green2015$Green.Votes_2015)
Green2015$Total.votes_2015 <- as.numeric(Green2015$Total.votes_2015)
Green2015$VoteShare_2015 <- Green2015$Green.Votes_2015/Green2015$Total.votes_2015*100
Green2015 <- Green2015[, -c(3,4,5)]
#2015 voting end

#2010 voting
Green2010 <- read.csv("Structured Data/greenVotes2010.csv")

Green2010 <- Green2010[, -c(3,4,5,6)]
Green2010$Green.Votes_2010 <- gsub(",", "", Green2010$Green.Votes_2010)
Green2010$Total.votes_2010 <- gsub(",", "", Green2010$Total.votes_2010)
Green2010$Green.Votes_2010 <- ifelse(Green2010$Green.Votes_2010 == "", 0, Green2010$Green.Votes_2010)
Green2010$Green.Votes_2010 <- as.numeric(Green2010$Green.Votes_2010)
Green2010$Total.votes_2010 <- as.numeric(Green2010$Total.votes_2010)
Green2010$VoteShare_2010 <- Green2010$Green.Votes_2010/Green2010$Total.votes_2010*100
Green2010 <- Green2010[, -c(3,4,5)]
#2010 voting end


#There are some differences in the naming of some constituencies in 2015 compared to the rest
#Therefore we order them by their id to see if they all have the same constituencies
Green2010<- Green2010[order(Green2010$id, decreasing=FALSE),]
Green2015<- Green2015[order(Green2015$id, decreasing=FALSE),]
Green2017<- Green2017[order(Green2017$id, decreasing=FALSE),]
Green2019<- Green2019[order(Green2019$id, decreasing=FALSE),]

identical(Green2017$id, Green2019$id)
identical(Green2017$id, Green2015$id)
identical(Green2017$id, Green2010$id)
#They are identical



#load Conservative voting data
#2019 voting

Cons2019 <- read_xlsx("Other Votes/consVotes2019.xlsx")

#remove irrelevant geographical information and electorate number 
Cons2019 <- Cons2019[, -c(3,4,5,6)]

colnames(Cons2019) <- c('id', 'Constituency', 'consVotes', 'consVoteShare', 'totalVotes', 'Year')
#replace empty cells with 0
Cons2019$consVotes <- ifelse(is.na(Cons2019$consVotes), 0, Cons2019$consVotes)
Cons2019$consVoteShare <- ifelse(is.na(Cons2019$consVoteShare), 0, Cons2019$consVoteShare)



#compare with the percentages in the document
#they match when rounded
#remove everything except for constituency code, name, year and vote share
Cons2019 <- Cons2019[, -c(3,5)]
#2019 voting end

#2017 voting
Cons2017 <- read_xlsx("Other Votes/consVotes2017.xlsx")
Cons2017 <- Cons2017[, -c(3,4,5,6)]
colnames(Cons2017) <- c('id', 'Constituency', 'consVotes', 'consVoteShare', 'totalVotes', 'Year')
Cons2017$consVotes <- ifelse(is.na(Cons2017$consVotes), 0, Cons2017$consVotes)
Cons2017$consVoteShare <- ifelse(is.na(Cons2017$consVoteShare), 0, Cons2017$consVoteShare)
Cons2017 <- Cons2017[, -c(3,5)]
#2017 voting end

#2015 voting
Cons2015 <- read_xlsx("Other Votes/consVotes2015.xlsx")
Cons2015 <- Cons2015[, -c(3,4,5,6)]
colnames(Cons2015) <- c('id', 'Constituency', 'consVotes', 'consVoteShare', 'totalVotes', 'Year')
Cons2015$consVotes <- ifelse(is.na(Cons2015$consVotes), 0, Cons2015$consVotes)
Cons2015$consVoteShare <- ifelse(is.na(Cons2015$consVoteShare), 0, Cons2015$consVoteShare)
Cons2015 <- Cons2015[, -c(3,5)]
#2015 voting end

#2010 voting
Cons2010 <- read_xlsx("Other Votes/consVotes2010.xlsx")
Cons2010 <- Cons2010[, -c(3,4,5,6)]
colnames(Cons2010) <- c('id', 'Constituency', 'consVotes', 'consVoteShare', 'totalVotes', 'Year')
Cons2010$consVotes <- ifelse(is.na(Cons2010$consVotes), 0, Cons2010$consVotes)
Cons2010$consVoteShare <- ifelse(is.na(Cons2010$consVoteShare), 0, Cons2010$consVoteShare)
Cons2010 <- Cons2010[, -c(3,5)]
#2010 voting end


#There are some differences in the naming of some constituencies in 2015 compared to the rest
#Therefore we order them by their id to see if they all have the same constituencies
Cons2010<- Cons2010[order(Cons2010$id, decreasing=FALSE),]
Cons2015<- Cons2015[order(Cons2015$id, decreasing=FALSE),]
Cons2017<- Cons2017[order(Cons2017$id, decreasing=FALSE),]
Cons2019<- Cons2019[order(Cons2019$id, decreasing=FALSE),]

identical(Cons2017$id, Cons2019$id)
identical(Cons2017$id, Cons2015$id)
identical(Cons2017$id, Cons2010$id)
#They are identical



#load Labour voting data
#2019 voting

Labour2019 <- read_xlsx("Other Votes/labourVotes2019.xlsx")

Labour2019 <- Labour2019[, -c(3,4,5,6)]
colnames(Labour2019) <- c('id', 'Constituency', 'LabourVotes', 'LabourVoteShare', 'totalVotes', 'Year')
Labour2019$LabourVotes <- ifelse(is.na(Labour2019$LabourVotes), 0, Labour2019$LabourVotes)
Labour2019$LabourVoteShare <- ifelse(is.na(Labour2019$LabourVoteShare), 0, Labour2019$LabourVoteShare)
Labour2019 <- Labour2019[, -c(3,5)]
#2019 voting end

#2017 voting
Labour2017 <- read_xlsx("Other Votes/labourVotes2017.xlsx")
Labour2017 <- Labour2017[, -c(3,4,5,6)]
colnames(Labour2017) <- c('id', 'Constituency', 'LabourVotes', 'LabourVoteShare', 'totalVotes', 'Year')
Labour2017$LabourVotes <- ifelse(is.na(Labour2017$LabourVotes), 0, Labour2017$LabourVotes)
Labour2017$LabourVoteShare <- ifelse(is.na(Labour2017$LabourVoteShare), 0, Labour2017$LabourVoteShare)
Labour2017 <- Labour2017[, -c(3,5)]
#2017 voting end

#2015 voting
Labour2015 <- read_xlsx("Other Votes/labourVotes2015.xlsx")
Labour2015 <- Labour2015[, -c(3,4,5,6)]
colnames(Labour2015) <- c('id', 'Constituency', 'LabourVotes', 'LabourVoteShare', 'totalVotes', 'Year')
Labour2015$LabourVotes <- ifelse(is.na(Labour2015$LabourVotes), 0, Labour2015$LabourVotes)
Labour2015$LabourVoteShare <- ifelse(is.na(Labour2015$LabourVoteShare), 0, Labour2015$LabourVoteShare)
Labour2015 <- Labour2015[, -c(3,5)]
#2015 voting end

#2010 voting
Labour2010 <- read_xlsx("Other Votes/labourVotes2010.xlsx")
Labour2010 <- Labour2010[, -c(3,4,5,6)]
colnames(Labour2010) <- c('id', 'Constituency', 'LabourVotes', 'LabourVoteShare', 'totalVotes', 'Year')
Labour2010$LabourVotes <- ifelse(is.na(Labour2010$LabourVotes), 0, Labour2010$LabourVotes)
Labour2010$LabourVoteShare <- ifelse(is.na(Labour2010$LabourVoteShare), 0, Labour2010$LabourVoteShare)
Labour2010 <- Labour2010[, -c(3,5)]
#2010 voting end


#There are some differences in the naming of some constituencies in 2015 compared to the rest
#Therefore we order them by their id to see if they all have the same constituencies
Labour2010<- Labour2010[order(Labour2010$id, decreasing=FALSE),]
Labour2015<- Labour2015[order(Labour2015$id, decreasing=FALSE),]
Labour2017<- Labour2017[order(Labour2017$id, decreasing=FALSE),]
Labour2019<- Labour2019[order(Labour2019$id, decreasing=FALSE),]

identical(Labour2017$id, Labour2019$id)
identical(Labour2017$id, Labour2015$id)
identical(Labour2017$id, Labour2010$id)
#They are identical

#load Libdem voting data
#2019 voting

Libdem2019 <- read_xlsx("Other Votes/libdemVotes2019.xlsx")

Libdem2019 <- Libdem2019[, -c(3,4,5,6)]
colnames(Libdem2019) <- c('id', 'Constituency', 'LibdemVotes', 'LibdemVoteShare', 'totalVotes', 'Year')
Libdem2019$LibdemVotes <- ifelse(is.na(Libdem2019$LibdemVotes), 0, Libdem2019$LibdemVotes)
Libdem2019$LibdemVoteShare <- ifelse(is.na(Libdem2019$LibdemVoteShare), 0, Libdem2019$LibdemVoteShare)
Libdem2019 <- Libdem2019[, -c(3,5)]
#2019 voting end

#2017 voting
Libdem2017 <- read_xlsx("Other Votes/libdemVotes2017.xlsx")
Libdem2017 <- Libdem2017[, -c(3,4,5,6)]
colnames(Libdem2017) <- c('id', 'Constituency', 'LibdemVotes', 'LibdemVoteShare', 'totalVotes', 'Year')
Libdem2017$LibdemVotes <- ifelse(is.na(Libdem2017$LibdemVotes), 0, Libdem2017$LibdemVotes)
Libdem2017$LibdemVoteShare <- ifelse(is.na(Libdem2017$LibdemVoteShare), 0, Libdem2017$LibdemVoteShare)
Libdem2017 <- Libdem2017[, -c(3,5)]
#2017 voting end

#2015 voting
Libdem2015 <- read_xlsx("Other Votes/libdemVotes2015.xlsx")
Libdem2015 <- Libdem2015[, -c(3,4,5,6)]
colnames(Libdem2015) <- c('id', 'Constituency', 'LibdemVotes', 'LibdemVoteShare', 'totalVotes', 'Year')
Libdem2015$LibdemVotes <- ifelse(is.na(Libdem2015$LibdemVotes), 0, Libdem2015$LibdemVotes)
Libdem2015$LibdemVoteShare <- ifelse(is.na(Libdem2015$LibdemVoteShare), 0, Libdem2015$LibdemVoteShare)
Libdem2015 <- Libdem2015[, -c(3,5)]
#2015 voting end

#2010 voting
Libdem2010 <- read_xlsx("Other Votes/libdemVotes2010.xlsx")
Libdem2010 <- Libdem2010[, -c(3,4,5,6)]
colnames(Libdem2010) <- c('id', 'Constituency', 'LibdemVotes', 'LibdemVoteShare', 'totalVotes', 'Year')
Libdem2010$LibdemVotes <- ifelse(is.na(Libdem2010$LibdemVotes), 0, Libdem2010$LibdemVotes)
Libdem2010$LibdemVoteShare <- ifelse(is.na(Libdem2010$LibdemVoteShare), 0, Libdem2010$LibdemVoteShare)
Libdem2010 <- Libdem2010[, -c(3,5)]
#2010 voting end


#There are some differences in the naming of some constituencies in 2015 compared to the rest
#Therefore we order them by their id to see if they all have the same constituencies
Libdem2010<- Libdem2010[order(Libdem2010$id, decreasing=FALSE),]
Libdem2015<- Libdem2015[order(Libdem2015$id, decreasing=FALSE),]
Libdem2017<- Libdem2017[order(Libdem2017$id, decreasing=FALSE),]
Libdem2019<- Libdem2019[order(Libdem2019$id, decreasing=FALSE),]

identical(Libdem2017$id, Libdem2019$id)
identical(Libdem2017$id, Libdem2015$id)
identical(Libdem2017$id, Libdem2010$id)
#They are identical



#load education control data
edu_2010 <- read.csv("Structured Data/edu_NWQ4+_2010.csv")
#Adjust column names to standardise
colnames(edu_2010) <- c('Constituency', 'id','Numerator_2010', 'Denominator_2010', 'Perc_NVQ4+', 'Conf_2010')
#recalculate percentages for more decimals
edu_2010$percHighEdu_2010 <- edu_2010$Numerator_2010/edu_2010$Denominator_2010*100
#remove redundant columns
edu_2010 <- edu_2010[, -c(3,4,5,6)]

edu_2015 <- read.csv("Structured Data/edu_NWQ4+_2015.csv")
colnames(edu_2015) <- c('Constituency', 'id','Numerator_2015', 'Denominator_2015', 'Perc_NVQ4+', 'Conf_2015')
edu_2015$percHighEdu_2015 <- edu_2015$Numerator_2015/edu_2015$Denominator_2015*100
edu_2015 <- edu_2015[, -c(3,4,5,6)]


edu_2017 <- read.csv("Structured Data/edu_NWQ4+_2017.csv")
colnames(edu_2017) <- c('Constituency', 'id','Numerator_2017', 'Denominator_2017', 'Perc_NVQ4+', 'Conf_2017')
edu_2017$percHighEdu_2017 <- edu_2017$Numerator_2017/edu_2017$Denominator_2017*100
edu_2017 <- edu_2017[, -c(3,4,5,6)]



edu_2019 <- read.csv("Structured Data/edu_NWQ4+_2019.csv")
colnames(edu_2019) <- c('Constituency', 'id','Numerator_2019', 'Denominator_2019', 'Perc_NVQ4+', 'Conf_2019')
edu_2019$percHighEdu_2019 <- edu_2019$Numerator_2019/edu_2019$Denominator_2019*100
edu_2019 <- edu_2019[, -c(3,4,5,6)]

#Load Median Age Data
age2010 <- read.csv("Structured Data/age2010.csv")
age2015 <- read.csv("Structured Data/age2015.csv")
age2017 <- read.csv("Structured Data/age2017.csv")
age2019 <- read.csv("Structured Data/age2019.csv")


density2010 <- read.csv("Structured Data/density_2010")
density2015 <- read.csv("Structured Data/density_2015")
density2017 <- read.csv("Structured Data/density_2017")
density2019 <- read.csv("Structured Data/density_2019")





#MERGING PROCESS
#merging 2010
merged_2010 <- merge(Green2010, Cons2010, by = "id")
merged_2010 <- merge(merged_2010, Labour2010, by = 'id')
merged_2010 <- merge(merged_2010, Libdem2010, by = 'id')
merged_2010 <- merge(merged_2010, pm2.5_2010, by = 'id')
merged_2010 <- merge(merged_2010, no2_2010, by = 'id')
merged_2010 <- merge(merged_2010, pm10_2010, by = 'id')
merged_2010 <- merge(merged_2010, so2_2010, by = 'id')
merged_2010 <- merge(merged_2010, oz_2010, by = 'id')
merged_2010 <- merge(merged_2010, edu_2010, by = 'id')
merged_2010 <- merge(merged_2010, age2010, by = 'id')
merged_2010 <- merge(merged_2010, density2010, by = 'id')

#deleting excess constituency columns
merged_2010 <- merged_2010[, -c(5,7,8,10,11,13,19)]

#merging 2015
merged_2015 <- merge(Green2015, Cons2015, by = "id")
merged_2015 <- merge(merged_2015, Labour2015, by = 'id')
merged_2015 <- merge(merged_2015, Libdem2015, by = 'id')
merged_2015 <- merge(merged_2015, pm2.5_2015, by = 'id')
merged_2015 <- merge(merged_2015, no2_2015, by = 'id')
merged_2015 <- merge(merged_2015, pm10_2015, by = 'id')
merged_2015 <- merge(merged_2015, so2_2015, by = 'id')
merged_2015 <- merge(merged_2015, oz_2015, by = 'id')
merged_2015 <- merge(merged_2015, edu_2015, by = 'id')
merged_2015 <- merge(merged_2015, age2015, by = 'id')
merged_2015 <- merge(merged_2015, density2015, by = 'id')


#deleting excess constituency columns
merged_2015 <- merged_2015[, -c(5,7,8,10,11,13,19)]

#merging 2017
merged_2017 <- merge(Green2017, Cons2017, by = "id")
merged_2017 <- merge(merged_2017, Labour2017, by = 'id')
merged_2017 <- merge(merged_2017, Libdem2017, by = 'id')
merged_2017 <- merge(merged_2017, pm2.5_2017, by = 'id')
merged_2017 <- merge(merged_2017, no2_2017, by = 'id')
merged_2017 <- merge(merged_2017, pm10_2017, by = 'id')
merged_2017 <- merge(merged_2017, so2_2017, by = 'id')
merged_2017 <- merge(merged_2017, oz_2017, by = 'id')
merged_2017 <- merge(merged_2017, edu_2017, by = 'id')
merged_2017 <- merge(merged_2017, age2017, by = 'id')
merged_2017 <- merge(merged_2017, density2017, by = 'id')

#deleting excess constituency columns
merged_2017 <- merged_2017[, -c(5,7,8,10,11,13,19)]

#merging 2019
merged_2019 <- merge(Green2019, Cons2019, by = "id")
merged_2019 <- merge(merged_2019, Labour2019, by = 'id')
merged_2019 <- merge(merged_2019, Libdem2019, by = 'id')
merged_2019 <- merge(merged_2019, pm2.5_2019, by = 'id')
merged_2019 <- merge(merged_2019, no2_2019, by = 'id')
merged_2019 <- merge(merged_2019, pm10_2019, by = 'id')
merged_2019 <- merge(merged_2019, so2_2019, by = 'id')
merged_2019 <- merge(merged_2019, oz_2019, by = 'id')
merged_2019 <- merge(merged_2019, edu_2019, by = 'id')
merged_2019 <- merge(merged_2019, age2019, by = 'id')
merged_2019 <- merge(merged_2019, density2019, by = 'id')

#deleting excess constituency columns
merged_2019 <- merged_2019[, -c(5,7,8,10,11,13,19)]

colnames(merged_2010) <- c('id', 'Constituency','Year','GreenVoteShare', 
                           'ConsVoteShare','LabourVoteShare', 
                           'LibdemVoteShare','pm2.5', 'no2', 'pm10', 'so2', 'oz',
                           'percHighEdu', 'age', 'density')
colnames(merged_2015) <- c('id', 'Constituency','Year','GreenVoteShare', 
                           'ConsVoteShare','LabourVoteShare', 
                           'LibdemVoteShare','pm2.5', 'no2', 'pm10', 'so2', 'oz',
                           'percHighEdu', 'age', 'density')
colnames(merged_2017) <- c('id', 'Constituency','Year','GreenVoteShare', 
                           'ConsVoteShare','LabourVoteShare', 
                           'LibdemVoteShare','pm2.5', 'no2', 'pm10', 'so2', 'oz',
                           'percHighEdu', 'age', 'density')
colnames(merged_2019) <- c('id', 'Constituency','Year','GreenVoteShare', 
                           'ConsVoteShare','LabourVoteShare', 
                           'LibdemVoteShare','pm2.5', 'no2', 'pm10', 'so2', 'oz',
                           'percHighEdu', 'age', 'density')


panelAll <- rbind(merged_2010, merged_2015, merged_2017, merged_2019)
panelAll<- panelAll[order(panelAll$id, decreasing=FALSE),]
panelAll<- subset(panelAll, panelAll$GreenVoteShare!=0)
panelAll<- subset(panelAll, panelAll$ConsVoteShare!=0)
panelAll<- subset(panelAll, panelAll$LabourVoteShare!=0)
panelAll<- subset(panelAll, panelAll$LibdemVoteShare!=0)

write.csv(panelAll, file = "Structured Data/panelAll.csv", row.names = FALSE)

