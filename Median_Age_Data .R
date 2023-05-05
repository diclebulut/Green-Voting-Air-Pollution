#To prepare the data
#I chose the corresponding years from the excel data of age distribution of all years
#Saved them as 4 seperate documents for 2010, 2015, 2017 and 2019
#deleted constituency name column
#added a row on the top for age categories 
#changed the colum names to be the same with the top row



library(dplyr)
library(matrixStats)
library(purrr)
install.packages("matrixStats")

scotAge2010 <- read.csv("Raw Age Data/Age_SC_2010.csv")
scotAge2015 <- read.csv("Raw Age Data/Age_SC_2015.csv")
scotAge2017 <- read.csv("Raw Age Data/Age_SC_2017.csv")
scotAge2019 <- read.csv("Raw Age Data/Age_SC_2019.csv")



scotAge2010_inv <- t(scotAge2010)
scotAge2015_inv <- t(scotAge2015)
scotAge2017_inv <- t(scotAge2017)
scotAge2019_inv <- t(scotAge2019)


medAge <- function(scotAge2010) {
scotAge2010_inv <- t(scotAge2010)

scotAge2010_inv <- scotAge2010_inv[-c(1,2),]
scotAge2010_inv <- as.data.frame(scotAge2010_inv)


# Convert columns 2 to 61 to numeric and remove commas
scotAge2010_inv[, 1:60] <- lapply(scotAge2010_inv[, 1:60], function(x) as.numeric(gsub(",", "", x)))

weighted_medians <- sapply(scotAge2010_inv[2:ncol(scotAge2010_inv)], function(x) {
  matrixStats::weightedMedian(scotAge2010_inv$V1, w = x)
})

new_row <- c("Weighted Median", weighted_medians)
scotAge2010_inv <- rbind(scotAge2010_inv, new_row)

scotAge2010_inv_inv <- t(scotAge2010_inv)
scotAge2010_inv_inv <- as.data.frame(scotAge2010_inv_inv)
scotAge2010_inv_inv$id <- scotAge2010$Id

scotMedianAge201 <- data.frame(id = scotAge2010_inv_inv$id, age = scotAge2010_inv_inv[,92])
scotMedianAge201 <- scotMedianAge201[-c(1),]
colnames(scotMedianAge201) <- c("id", "age")

write.csv(scotMedianAge201, file = "median.csv", row.names = FALSE)}

#After applying each of them, go and change the document name and proceed to the other
medAge(scotAge2010)
medAge(scotAge2015)
medAge(scotAge2017)
medAge(scotAge2019)




#Merging

medianAgeEngWalNI <- read.csv("Raw Age Data/Age_En_Wal_NI.csv")
medianAgeEngWalNI <- medianAgeEngWalNI[,-c(2)]
medianAgeEngWalNI2010 <- data.frame(id = medianAgeEngWalNI$pcon11nm, age = medianAgeEngWalNI[,2])
medianAgeEngWalNI2015 <- data.frame(id = medianAgeEngWalNI$pcon11nm, age = medianAgeEngWalNI[,3])
medianAgeEngWalNI2017 <- data.frame(id = medianAgeEngWalNI$pcon11nm, age = medianAgeEngWalNI[,4])
medianAgeEngWalNI2019 <- data.frame(id = medianAgeEngWalNI$pcon11nm, age = medianAgeEngWalNI[,5])


sc2010 <-read.csv("Raw Age Data/medianScot2010.csv")
sc2015 <-read.csv("Raw Age Data/medianScot2015.csv")
sc2017 <-read.csv("Raw Age Data/medianScot2017.csv")
sc2019 <-read.csv("Raw Age Data/medianScot2019.csv")

age2010 <- rbind(medianAgeEngWalNI2010, sc2010)
age2015 <- rbind(medianAgeEngWalNI2015, sc2015)
age2017 <- rbind(medianAgeEngWalNI2017, sc2017)
age2019 <- rbind(medianAgeEngWalNI2019, sc2019)

write.csv(age2010, file = "Structured Data/age2010.csv", row.names = FALSE)
write.csv(age2015, file = "Structured Data/age2015.csv", row.names = FALSE)
write.csv(age2017, file = "Structured Data/age2017.csv", row.names = FALSE)
write.csv(age2019, file = "Structured Data/age2019.csv", row.names = FALSE)





