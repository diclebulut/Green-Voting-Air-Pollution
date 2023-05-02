##6.0 version notes
#This version only includes models
#No creation code in this version
#For air pollution creation code see Version 4.5
#For panel creation code see Version 5.5
#check previous versions for additional notes




library(plm)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(geojsonio)
library(lmtest)


#loading the pre made panel data 
panel <- read.csv("Structured Data/panel.csv")
panelDataConsOnly <- pdata.frame(panel, index = c("id"))
panelDataTimeOnly <- pdata.frame(panel, index = c("Year"))
panelData <- pdata.frame(panel, index = c("id", "Year"))



#EXPERIMENTAL LM MODEL
lm_base_pm2.5 <- lm(voteShare ~ pm2.5, data = panel)
lm_base_no2 <- lm(voteShare ~ no2, data = panel)
lm_base_pm10 <- lm(voteShare ~ pm10, data = panel)
lm_base_so2 <- lm(voteShare ~ so2, data = panel)
lm_base_bz <- lm(voteShare ~ bz, data = panel)

lm_pm2.5 <- lm(voteShare ~ pm2.5 + percHighEdu + medianIncome, data = panel)
lm_no2 <- lm(voteShare ~ no2 + percHighEdu + medianIncome, data = panel)
lm_pm10 <- lm(voteShare ~ pm10 + percHighEdu + medianIncome, data = panel)
lm_so2 <- lm(voteShare ~ so2 + percHighEdu + medianIncome, data = panel)
lm_bz <- lm(voteShare ~ bz + percHighEdu + medianIncome, data = panel)

summary(lm_base_pm2.5)
summary(lm_base_no2)
summary(lm_base_pm10)
summary(lm_base_so2)
summary(lm_base_bz)


summary(lm_pm2.5)
summary(lm_no2)
summary(lm_pm10)
summary(lm_so2)
summary(lm_bz)


#FIXED EFFECTS MODELS pm2.5 

fe_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu + medianIncome, data = panelData, model = "within",  effect = "twoways")
fe_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelData, model = "within", effect = "twoways")

summary(fe_pm2.5)
summary(fe_pm2.5_base)


#FIXED EFFECTS MODELS no2 

fe_no2 <- plm(voteShare ~ no2 + percHighEdu + medianIncome, data = panelData, model = "within", effect = "twoways")
fe_no2_base <- plm(voteShare ~ no2, data = panelData, model = "within", effect = "twoways")

summary(fe_no2)
summary(fe_no2_base)

#FIXED EFFECTS MODELS PM10 

fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu + medianIncome, data = panelData, model = "within", effect = "twoways")
fe_pm10_base <- plm(voteShare ~ pm10, data = panelData, model = "within", effect = "twoways")

summary(fe_pm10)
summary(fe_pm10_base)


#FIXED EFFECTS MODELS SO2

fe_so2 <- plm(voteShare ~ so2 + percHighEdu + medianIncome, data = panelData, model = "within", effect = "twoways")
fe_so2_base <- plm(voteShare ~ so2, data = panelData, model = "within", effect = "twoways")

summary(fe_so2)
summary(fe_so2_base)


#FIXED EFFECTS MODELS BZ

fe_bz <- plm(voteShare ~ bz + percHighEdu + medianIncome, data = panelData, model = "within", effect = "twoways")
fe_bz_base <- plm(voteShare ~ bz, data = panelData, model = "within", effect = "twoways")

summary(fe_bz)
summary(fe_bz_base)




#FIXED EFFECTS MODELS With only id index PM10 

fe_id_pm10<- plm(voteShare ~ pm10 + percHighEdu + medianIncome, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_pm10_base <- plm(voteShare ~ pm10, data = panelDataConsOnly, model = "within", effect = "twoways")

summary(fe_id_pm10)
summary(fe_id_pm10_base)

#FIXED EFFECTS MODELS With only time index PM10 

fe_time_pm10<- plm(voteShare ~ pm10 + percHighEdu + medianIncome, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_pm10_base <- plm(voteShare ~ pm10, data = panelDataTimeOnly, model = "within", effect = "twoways")

summary(fe_time_pm10)
summary(fe_time_pm10_base)


