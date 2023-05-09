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
library(gplots)
library(psych)



#loading the pre made panel data 
panel <- read.csv("Structured Data/panel.csv")
panelDataConsOnly <- pdata.frame(panel, index = c("id"))
panelDataTimeOnly <- pdata.frame(panel, index = c("Year"))
panelData <- pdata.frame(panel, index = c("id", "Year"))



#EXPERIMENTAL LM MODEL
lm_base_pm2.5 <- lm(voteShare ~ pm2.5, data = panelData)
lm_base_no2 <- lm(voteShare ~ no2, data = panelData)
lm_base_pm10 <- lm(voteShare ~ pm10, data = panelData)
lm_base_so2 <- lm(voteShare ~ so2, data = panelData)
lm_base_oz <- lm(voteShare ~ oz, data = panelData)

lm_pm2.5 <- lm(voteShare ~ pm2.5 + percHighEdu + medianAge, data = panelData)
lm_no2 <- lm(voteShare ~ no2 + percHighEdu +  medianAge, data = panelData)
lm_pm10 <- lm(voteShare ~ pm10 + percHighEdu + medianAge, data = panelData)
lm_so2 <- lm(voteShare ~ so2 + percHighEdu + medianAge, data = panelData)
lm_oz <- lm(voteShare ~ oz + percHighEdu +  medianAge, data = panelData)

summary(lm_base_pm2.5)
summary(lm_base_no2)
summary(lm_base_pm10)
summary(lm_base_so2)
summary(lm_base_oz)


summary(lm_pm2.5)
summary(lm_no2)
summary(lm_pm10)
summary(lm_so2)
summary(lm_oz)


coeftest(lm_base_pm2.5, vcov = vcovHC(lm_base_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_base_no2, vcov = vcovHC(lm_base_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_base_pm10, vcov = vcovHC(lm_base_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_base_so2, vcov = vcovHC(lm_base_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_base_oz, vcov = vcovHC(lm_base_oz, type = "HC1", method = "white2", cluster = "group"))


coeftest(lm_pm2.5, vcov = vcovHC(lm_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_no2, vcov = vcovHC(lm_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_pm10, vcov = vcovHC(lm_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_so2, vcov = vcovHC(lm_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(lm_oz, vcov = vcovHC(lm_oz, type = "HC1", method = "white2", cluster = "group"))

#FIXED EFFECTS MODELS pm2.5 

fe_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu +  medianAge, data = panelData, model = "within",  effect = "twoways")
fe_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm2.5, vcov = vcovHC(fe_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm2.5_base, vcov = vcovHC(fe_pm2.5_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm2.5)
summary(fe_pm2.5_base)

fe_id_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu + medianAge, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelDataConsOnly, model = "within", effect = "twoways")
coeftest(fe_id_pm2.5, vcov = vcovHC(fe_id_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_id_pm2.5_base, vcov = vcovHC(fe_id_pm2.5_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_id_pm2.5)
summary(fe_id_pm2.5_base)

fe_time_pm2.5<- plm(voteShare ~ pm2.5 + percHighEdu +  medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_pm2.5_base <- plm(voteShare ~ pm2.5, data = panelDataTimeOnly, model = "within", effect = "twoways")
coeftest(fe_time_pm2.5, vcov = vcovHC(fe_time_pm2.5, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_time_pm2.5_base, vcov = vcovHC(fe_time_pm2.5_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_time_pm2.5)
summary(fe_time_pm2.5_base)

#FIXED EFFECTS MODELS no2 

fe_no2 <- plm(voteShare ~ no2 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_no2_base <- plm(voteShare ~ no2, data = panelData, model = "within", effect = "twoways")
coeftest(fe_no2, vcov = vcovHC(fe_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_no2_base, vcov = vcovHC(fe_no2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_no2)
summary(fe_no2_base)

fe_id_no2<- plm(voteShare ~ no2 + percHighEdu + medianAge, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_no2_base <- plm(voteShare ~ no2, data = panelDataConsOnly, model = "within", effect = "twoways")
coeftest(fe_id_no2, vcov = vcovHC(fe_id_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_id_no2_base, vcov = vcovHC(fe_id_no2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_id_no2)
summary(fe_id_no2_base)

fe_time_no2<- plm(voteShare ~ no2 + percHighEdu +  medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_no2_base <- plm(voteShare ~ no2, data = panelDataTimeOnly, model = "within", effect = "twoways")
coeftest(fe_time_no2, vcov = vcovHC(fe_time_no2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_time_no2_base, vcov = vcovHC(fe_time_no2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_time_no2)
summary(fe_time_no2_base)

#FIXED EFFECTS MODELS PM10 

fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu +  medianAge, data = panelData, model = "within", effect = "twoways")
fe_pm10_base <- plm(voteShare ~ pm10, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm10, vcov = vcovHC(fe_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_pm10_base, vcov = vcovHC(fe_pm10_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm10)
summary(fe_pm10_base)

fe_id_pm10<- plm(voteShare ~ pm10 + percHighEdu + medianAge, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_pm10_base <- plm(voteShare ~ pm10, data = panelDataConsOnly, model = "within", effect = "twoways")
coeftest(fe_id_pm10, vcov = vcovHC(fe_id_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_id_pm10_base, vcov = vcovHC(fe_id_pm10_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_id_pm10)
summary(fe_id_pm10_base)

fe_time_pm10<- plm(voteShare ~ pm10 + percHighEdu +  medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_pm10_base <- plm(voteShare ~ pm10, data = panelDataTimeOnly, model = "within", effect = "twoways")
coeftest(fe_time_pm10, vcov = vcovHC(fe_time_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_time_pm10, vcov = vcovHC(fe_time_pm10, type = "HC1", method = "white2", cluster = "group"))

summary(fe_time_pm10)
summary(fe_time_pm10_base)

#FIXED EFFECTS MODELS SO2

fe_so2 <- plm(voteShare ~ so2 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_so2_base <- plm(voteShare ~ so2, data = panelData, model = "within", effect = "twoways")
coeftest(fe_so2, vcov = vcovHC(fe_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_so2_base, vcov = vcovHC(fe_so2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_so2)
summary(fe_so2_base)

fe_id_so2<- plm(voteShare ~ so2 + percHighEdu + medianAge, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_so2_base <- plm(voteShare ~ so2, data = panelDataConsOnly, model = "within", effect = "twoways")
coeftest(fe_id_so2, vcov = vcovHC(fe_id_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_id_so2_base, vcov = vcovHC(fe_id_so2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_id_so2)
summary(fe_id_so2_base)

fe_time_so2<- plm(voteShare ~ so2 + percHighEdu +  medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_so2_base <- plm(voteShare ~ so2, data = panelDataTimeOnly, model = "within", effect = "twoways")
coeftest(fe_time_so2, vcov = vcovHC(fe_time_so2, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_time_so2_base, vcov = vcovHC(fe_time_so2_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_time_so2)
summary(fe_time_so2_base)



#FIXED EFFECTS MODELS OZ

fe_oz <- plm(voteShare ~ oz + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")
fe_oz_base <- plm(voteShare ~ oz, data = panelData, model = "within", effect = "twoways")
coeftest(fe_oz, vcov = vcovHC(fe_oz, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_oz_base, vcov = vcovHC(fe_oz_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_oz)
summary(fe_oz_base)

fe_id_oz<- plm(voteShare ~ oz + percHighEdu + medianAge, data = panelDataConsOnly, model = "within", effect = "twoways")
fe_id_oz_base <- plm(voteShare ~ oz, data = panelDataConsOnly, model = "within", effect = "twoways")
coeftest(fe_id_oz, vcov = vcovHC(fe_id_oz, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_id_oz_base, vcov = vcovHC(fe_id_oz_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_id_oz)
summary(fe_id_oz_base)

fe_time_oz<- plm(voteShare ~ oz + percHighEdu +  medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_oz_base <- plm(voteShare ~ oz, data = panelDataTimeOnly, model = "within", effect = "twoways")
coeftest(fe_time_oz, vcov = vcovHC(fe_time_oz, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_time_oz_base, vcov = vcovHC(fe_time_oz_base, type = "HC1", method = "white2", cluster = "group"))

summary(fe_time_oz)
summary(fe_time_oz_base)

#FIXED EFFECTS MODELS edu

fe_edu <- plm(voteShare ~ percHighEdu, data = panelData, model = "within", effect = "twoways")
coeftest(fe_edu, vcov = vcovHC(fe_edu, type = "HC1", method = "white2", cluster = "group"))

summary(fe_edu)

#FIXED EFFECTS MODELS age

fe_age <- plm(voteShare ~ medianAge, data = panelData, model = "within", effect = "twoways")
coeftest(fe_age, vcov = vcovHC(fe_age, type = "HC1", method = "white2", cluster = "group"))

summary(fe_age)





#FIXED EFFECTS MODELS With only time index PM10 

fe_time_pm10<- plm(voteShare ~ pm10 + percHighEdu +  medianAge, data = panelDataTimeOnly, model = "within", effect = "twoways")
fe_time_pm10_base <- plm(voteShare ~ pm10, data = panelDataTimeOnly, model = "within", effect = "twoways")
coeftest(fe_time_pm10, vcov = vcovHC(fe_time_pm10, type = "HC1", method = "white2", cluster = "group"))
coeftest(fe_time_pm10, vcov = vcovHC(fe_time_pm10, type = "HC1", method = "white2", cluster = "group"))

summary(fe_time_pm10)
summary(fe_time_pm10_base)


#########
#Descriptive stats

summary(panel)
describe(panel)



########
#Robustness test with density#
fe_pm10_rob <- plm(pm10 ~  popDensity, data = panelData, model = "within", effect = "twoways")
coeftest(fe_pm10_rob, vcov = vcovHC(fe_pm10_rob, type = "HC1", method = "white2", cluster = "group"))

summary(fe_pm10_rob)

fe_rob_2 <- plm(voteShare ~  popDensity, data = panelData, model = "within", effect = "twoways")
coeftest(fe_rob_2, vcov = vcovHC(fe_rob_2, type = "HC1", method = "white2", cluster = "group"))

summary(fe_rob_2)

library("sandwich")

# Robust t test
coeftest(fe_pm10, vcov = vcovHC(fe_pm10, type = "HC1", method = "white2", cluster = "group"))

##### DRRISCOLL AND KRAAY ######
coeftest(fe_pm10, vcov = vcovSCC(fe_pm10, type = "HC1", cluster = "group"))
coeftest(fe_pm2.5, vcov = vcovSCC(fe_pm2.5, type = "HC1", cluster = "group"))
coeftest(fe_no2, vcov = vcovSCC(fe_no2, type = "HC1", cluster = "group"))
coeftest(fe_so2, vcov = vcovSCC(fe_so2, type = "HC1", cluster = "group"))
coeftest(fe_oz, vcov = vcovSCC(fe_oz, type = "HC1", cluster = "group"))


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


#HAUSMAN TEST
# Fit the random effects model
re_model <- plm(voteShare ~ pm10 + percHighEdu +  medianAge, data = panelData, model = "random")
fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu + medianAge, data = panelData, model = "within", effect = "twoways")

# Print the model summary
phtest(fe_pm10, model)

#WE REJECT THE RANDOM EFFECTS MODEL


#FD
fd3 <- plm(voteShare  ~ pm10 + percHighEdu + medianAge - 1, 
           data = panelData, model = "fd")
summary(fd3)
summary(fe_pm10)

#Residuals has a conditional mean zero - test
fe_residuals_pm10 <- residuals(fe_pm10_base)
mean(fe_residuals_pm10)
lm_residuals_pm10 <- lm(fe_residuals_pm10 ~ pm10 , data = panelData)
summary(lm_residuals_pm10)



fe_residuals_pm2.5 <- residuals(fe_pm2.5_base)
mean(fe_residuals_pm2.5)
lm_residuals_pm2.5<- lm(fe_residuals_pm2.5 ~ pm2.5 , data = panelData)
summary(lm_residuals_pm2.5)


fe_residuals_no2 <- residuals(fe_no2_base)
mean(fe_residuals_no2)
lm_residuals_no2 <- lm(fe_residuals_no2 ~ no2 , data = panelData)
summary(lm_residuals_no2)


fe_residuals_so2 <- residuals(fe_so2_base)
mean(fe_residuals_so2)
lm_residuals_so2 <- lm(fe_residuals_so2 ~ so2 , data = panelData)
summary(lm_residuals_so2)

fe_residuals_oz<- residuals(fe_oz_base)
mean(fe_residuals_oz)
lm_residuals_oz <- lm(fe_residuals_oz ~ oz , data = panelData)
summary(lm_residuals_oz)

install.packages("gplots")
gplots::plotmeans(voteShare ~ id, main="Heterogeneity Across Constituencies", data=panelData, xlab = "Constituency ID", ylab="Green Party Vote Share")
gplots::plotmeans(voteShare ~ Year, main="Heterogeneity Across Periods", data=panelData, xlab= "Years", ylab="Green Party Vote Share" )


