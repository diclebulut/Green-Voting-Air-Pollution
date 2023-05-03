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

#bz
bz_2010 <- read.csv("Structured Data/avg_bz_2010.csv")
colnames(bz_2010) <- c('id', 'bz')
bz_2015 <- read.csv("Structured Data/avg_bz_2015.csv")
colnames(bz_2015) <- c('id', 'bz')
bz_2017 <- read.csv("Structured Data/avg_bz_2017.csv")
colnames(bz_2017) <- c('id', 'bz')
bz_2019 <- read.csv("Structured Data/avg_bz_2019.csv")
colnames(bz_2019) <- c('id', 'bz')

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


#load income data
income_2010 <- read.csv("Structured Data/income median_2010.csv")
colnames(income_2010) <- c('Constituency', 'id','medianIncome_2010')
income_2010 <- income_2010[, -c(4)]

income_2015 <- read.csv("Structured Data/income median_2015.csv")
colnames(income_2015) <- c('Constituency', 'id','medianIncome_2015')
income_2015 <- income_2015[, -c(4)]


income_2017 <- read.csv("Structured Data/income median_2017.csv")
colnames(income_2017) <- c('Constituency', 'id','medianIncome_2017')
income_2017 <- income_2017[, -c(4)]


income_2019 <- read.csv("Structured Data/income median_2019.csv")
colnames(income_2019) <- c('Constituency', 'id','medianIncome_2010')
income_2019 <- income_2019[, -c(4)]


#MERGING PROCESS
#merging 2010
merged_2010 <- merge(Green2010, pm2.5_2010, by = "id")
merged_2010 <- merge(merged_2010, no2_2010, by = 'id')
merged_2010 <- merge(merged_2010, pm10_2010, by = 'id')
merged_2010 <- merge(merged_2010, so2_2010, by = 'id')
merged_2010 <- merge(merged_2010, bz_2010, by = 'id')
merged_2010 <- merge(merged_2010, oz_2010, by = 'id')
merged_2010 <- merge(merged_2010, edu_2010, by = 'id')
merged_2010 <- merge(merged_2010, income_2010, by = 'id')

#deleting excess constituency columns
merged_2010 <- merged_2010[, -c(11,13)]

#merging 2015
merged_2015 <- merge(Green2015, pm2.5_2015, by = "id")
merged_2015 <- merge(merged_2015, no2_2015, by = 'id')
merged_2015 <- merge(merged_2015, pm10_2015, by = 'id')
merged_2015 <- merge(merged_2015, so2_2015, by = 'id')
merged_2015 <- merge(merged_2015, bz_2015, by = 'id')
merged_2015 <- merge(merged_2015, oz_2015, by = 'id')
merged_2015 <- merge(merged_2015, edu_2015, by = 'id')
merged_2015 <- merge(merged_2015, income_2015, by = 'id')

#deleting excess constituency columns
merged_2015 <- merged_2015[, -c(11,13)]

#merging 2017
merged_2017 <- merge(Green2017, pm2.5_2017, by = "id")
merged_2017 <- merge(merged_2017, no2_2017, by = 'id')
merged_2017 <- merge(merged_2017, pm10_2017, by = 'id')
merged_2017 <- merge(merged_2017, so2_2017, by = 'id')
merged_2017 <- merge(merged_2017, bz_2017, by = 'id')
merged_2017 <- merge(merged_2017, oz_2017, by = 'id')
merged_2017 <- merge(merged_2017, edu_2017, by = 'id')
merged_2017 <- merge(merged_2017, income_2017, by = 'id')

#deleting excess constituency columns
merged_2017 <- merged_2017[, -c(11,13)]

#merging 2019
merged_2019 <- merge(Green2019, pm2.5_2019, by = "id")
merged_2019 <- merge(merged_2019, no2_2019, by = 'id')
merged_2019 <- merge(merged_2019, pm10_2019, by = 'id')
merged_2019 <- merge(merged_2019, so2_2019, by = 'id')
merged_2019 <- merge(merged_2019, bz_2019, by = 'id')
merged_2019 <- merge(merged_2019, oz_2019, by = 'id')
merged_2019 <- merge(merged_2019, edu_2019, by = 'id')
merged_2019 <- merge(merged_2019, income_2019, by = 'id')

#deleting excess constituency columns
merged_2019 <- merged_2019[, -c(11,13)]

colnames(merged_2010) <- c('id', 'Constituency','Year','voteShare','pm2.5', 'no2', 'pm10','so2', 'bz', 'oz', 'percHighEdu', 'medianIncome')
colnames(merged_2015) <- c('id', 'Constituency','Year','voteShare','pm2.5', 'no2', 'pm10', 'so2', 'bz','oz', 'percHighEdu', 'medianIncome')
colnames(merged_2017) <- c('id', 'Constituency','Year','voteShare','pm2.5', 'no2', 'pm10', 'so2', 'bz','oz','percHighEdu', 'medianIncome')
colnames(merged_2019) <- c('id', 'Constituency','Year','voteShare','pm2.5', 'no2', 'pm10', 'so2', 'bz','oz','percHighEdu', 'medianIncome')


panel <- rbind(merged_2010, merged_2015, merged_2017, merged_2019)
panel<- panel[order(panel$id, decreasing=FALSE),]
panel$medianIncome <- as.numeric(panel$medianIncome)
panel<- subset(panel, !(is.na(panel$medianIncome)))
panel<- subset(panel, panel$voteShare!=0)

write.csv(panel, file = "Structured Data/panel.csv", row.names = FALSE)

