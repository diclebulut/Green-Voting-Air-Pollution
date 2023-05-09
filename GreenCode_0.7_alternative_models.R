library(plm)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(geojsonio)
library(lmtest)
library(car)
library(gplots)
library(psych)


######INCLUDING ZERO VOTES#######
#loading the pre made panel data 
panel <- read.csv("Structured Data/panel_0_votes.csv")
panelData <- pdata.frame(panel, index = c("id", "Year"))


#FIXED EFFECTS MODELS PM10 

fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu +  medianAge, data = panelData, model = "within", effect = "twoways")
fe_pm10_base <- plm(voteShare ~ pm10, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm10, vcov = vcovHC(fe_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm10_base, vcov = vcovHC(fe_pm10_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm10)
summary(fe_pm10_base)

#FIXED EFFECTS MODELS pm2.5 

fe_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu +  medianAge, data = panelData, model = "within",  effect = "twoways")
fe_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm2.5, vcov = vcovHC(fe_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm2.5_base, vcov = vcovHC(fe_pm2.5_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm2.5)
summary(fe_pm2.5_base)


#FIXED EFFECTS MODELS no2 

fe_no2 <- plm(voteShare ~ no2 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_no2_base <- plm(voteShare ~ no2, data = panelData, model = "within", effect = "twoways")
coeftest(fe_no2, vcov = vcovHC(fe_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_no2_base, vcov = vcovHC(fe_no2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_no2)
summary(fe_no2_base)

#FIXED EFFECTS MODELS SO2

fe_so2 <- plm(voteShare ~ so2 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_so2_base <- plm(voteShare ~ so2, data = panelData, model = "within", effect = "twoways")
coeftest(fe_so2, vcov = vcovHC(fe_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_so2_base, vcov = vcovHC(fe_so2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_so2)
summary(fe_so2_base)

#FIXED EFFECTS MODELS OZ

fe_oz <- plm(voteShare ~ oz + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_oz_base <- plm(voteShare ~ oz, data = panelData, model = "within", effect = "twoways")
coeftest(fe_oz, vcov = vcovHC(fe_oz, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_oz_base, vcov = vcovHC(fe_oz_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_oz)
summary(fe_oz_base)


######## REMOVING OUTLIERS ###########
#loading the pre made panel data 
panel <- read.csv("Structured Data/panel.csv")
boxplot(panel$voteShare)

# Calculate the quartiles and IQR
Q1 <- quantile(panel$voteShare, 0.25)
Q3 <- quantile(panel$voteShare, 0.75)
IQR <- Q3 - Q1

# Calculate the fence values

UF <- Q3 + 1.5 * IQR
UF

panel <- panel[panel$voteShare <= UF, ]
panelData <- pdata.frame(panel, index = c("id", "Year"))


#FIXED EFFECTS MODELS PM10 

fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu +  medianAge, data = panelData, model = "within", effect = "twoways")
fe_pm10_base <- plm(voteShare ~ pm10, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm10, vcov = vcovHC(fe_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm10_base, vcov = vcovHC(fe_pm10_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm10)
summary(fe_pm10_base)

#FIXED EFFECTS MODELS pm2.5 

fe_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu +  medianAge, data = panelData, model = "within",  effect = "twoways")
fe_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm2.5, vcov = vcovHC(fe_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm2.5_base, vcov = vcovHC(fe_pm2.5_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm2.5)
summary(fe_pm2.5_base)


#FIXED EFFECTS MODELS no2 

fe_no2 <- plm(voteShare ~ no2 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_no2_base <- plm(voteShare ~ no2, data = panelData, model = "within", effect = "twoways")
coeftest(fe_no2, vcov = vcovHC(fe_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_no2_base, vcov = vcovHC(fe_no2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_no2)
summary(fe_no2_base)

#FIXED EFFECTS MODELS SO2

fe_so2 <- plm(voteShare ~ so2 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_so2_base <- plm(voteShare ~ so2, data = panelData, model = "within", effect = "twoways")
coeftest(fe_so2, vcov = vcovHC(fe_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_so2_base, vcov = vcovHC(fe_so2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_so2)
summary(fe_so2_base)

#FIXED EFFECTS MODELS OZ

fe_oz <- plm(voteShare ~ oz + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_oz_base <- plm(voteShare ~ oz, data = panelData, model = "within", effect = "twoways")
coeftest(fe_oz, vcov = vcovHC(fe_oz, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_oz_base, vcov = vcovHC(fe_oz_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_oz)
summary(fe_oz_base)


