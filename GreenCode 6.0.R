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
library(car)


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
lm_base_oz <- lm(voteShare ~ oz, data = panel)

lm_pm2.5 <- lm(voteShare ~ pm2.5 + percHighEdu + medianIncome + medianAge, data = panel)
lm_no2 <- lm(voteShare ~ no2 + percHighEdu + medianIncome + medianAge, data = panel)
lm_pm10 <- lm(voteShare ~ pm10 + percHighEdu + medianIncome + medianAge, data = panel)
lm_so2 <- lm(voteShare ~ so2 + percHighEdu + medianIncome + medianAge, data = panel)
lm_bz <- lm(voteShare ~ bz + percHighEdu + medianIncome + medianAge, data = panel)
lm_oz <- lm(voteShare ~ oz + percHighEdu + medianIncome + medianAge, data = panel)

summary(lm_base_pm2.5)
summary(lm_base_no2)
summary(lm_base_pm10)
summary(lm_base_so2)
summary(lm_base_bz)
summary(lm_base_oz)


summary(lm_pm2.5)
summary(lm_no2)
summary(lm_pm10)
summary(lm_so2)
summary(lm_bz)
summary(lm_oz)

#FIXED EFFECTS MODELS pm2.5 

fe_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu + medianIncome + medianAge, data = panelData, model = "within",  effect = "twoways")
fe_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelData, model = "within", effect = "twoways")

summary(fe_pm2.5)
summary(fe_pm2.5_base)


#FIXED EFFECTS MODELS no2 

fe_no2 <- plm(voteShare ~ no2 + percHighEdu + medianIncome + medianAge, data = panelData, model = "within", effect = "twoways")
fe_no2_base <- plm(voteShare ~ no2, data = panelData, model = "within", effect = "twoways")

summary(fe_no2)
summary(fe_no2_base)

#FIXED EFFECTS MODELS PM10 

fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu + medianIncome + medianAge, data = panelData, model = "within", effect = "twoways")
fe_pm10_base <- plm(voteShare ~ pm10, data = panelData, model = "within", effect = "twoways")

summary(fe_pm10)
summary(fe_pm10_base)


#FIXED EFFECTS MODELS SO2

fe_so2 <- plm(voteShare ~ so2 + percHighEdu + medianIncome + medianAge, data = panelData, model = "within", effect = "twoways")
fe_so2_base <- plm(voteShare ~ so2, data = panelData, model = "within", effect = "twoways")

summary(fe_so2)
summary(fe_so2_base)


#FIXED EFFECTS MODELS BZ

fe_bz <- plm(voteShare ~ bz + percHighEdu + medianIncome + medianAge, data = panelData, model = "within", effect = "twoways")
fe_bz_base <- plm(voteShare ~ bz, data = panelData, model = "within", effect = "twoways")

summary(fe_bz)
summary(fe_bz_base)

#FIXED EFFECTS MODELS OZ

fe_oz <- plm(voteShare ~ oz + percHighEdu + medianIncome + medianAge, data = panelData, model = "within", effect = "twoways")
fe_oz_base <- plm(voteShare ~ oz, data = panelData, model = "within", effect = "twoways")

summary(fe_oz)
summary(fe_oz_base)



#FIXED EFFECTS MODELS With only id index PM10 

fe_id_pm10<- plm(voteShare ~ pm10 + percHighEdu + medianIncome + medianAge, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_pm10_base <- plm(voteShare ~ pm10, data = panelDataConsOnly, model = "within", effect = "twoways")

summary(fe_id_pm10)
summary(fe_id_pm10_base)

#FIXED EFFECTS MODELS With only time index PM10 

fe_time_pm10<- plm(voteShare ~ pm10 + percHighEdu + medianIncome + medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_pm10_base <- plm(voteShare ~ pm10, data = panelDataTimeOnly, model = "within", effect = "twoways")

summary(fe_time_pm10)
summary(fe_time_pm10_base)





library("sandwich")

# Robust t test
coeftest(fe_pm10, vcov = vcovHC(fe_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm10, vcov = vcovSCC(fe_pm10, type = "HC1", cluster = "group"))

#Histogram of residuals
g1 <- qplot(fe_pm10$residuals,
            geom = "histogram",
            bins = 10) +
  labs(title = "Histogram of residuals",
       x = "residual")
g1

###
ggplot() +
  geom_qq(aes(sample = residuals(fe_pm10))) +
  geom_abline(color = "red") +
  coord_fixed()

qqnorm(residuals(fe_pm10, pch = 1, frame = FALSE))
qqline(residuals(fe_pm10, col = "red", lwd = 2))
# thin tailed distribution

