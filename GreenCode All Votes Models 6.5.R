##6.5 version notes
#This version only includes models for votes of all parties
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
panelAll <- read.csv("Structured Data/panelAll.csv")
panelDataAll<- pdata.frame(panelAll, index = c("id", "Year"))



#EXPERIMENTAL LM MODEL Green
lm_base_pm2.5 <- lm(GreenVoteShare ~ pm2.5, data = panelAll)
lm_base_no2 <- lm(GreenVoteShare ~ no2, data = panelAll)
lm_base_pm10 <- lm(GreenVoteShare ~ pm10, data = panelAll)
lm_base_so2 <- lm(GreenVoteShare ~ so2, data = panelAll)
lm_base_oz <- lm(GreenVoteShare ~ oz, data = panelAll)

lm_pm2.5 <- lm(GreenVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelAll)
lm_no2 <- lm(GreenVoteShare ~ no2 + percHighEdu + medianAge, data = panelAll)
lm_pm10 <- lm(GreenVoteShare ~ pm10 + percHighEdu + medianAge, data = panelAll)
lm_so2 <- lm(GreenVoteShare ~ so2 + percHighEdu + medianAge, data = panelAll)
lm_oz <- lm(GreenVoteShare ~ oz + percHighEdu + medianAge, data = panelAll)

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


#EXPERIMENTAL LM MODEL Conservative
lm_base_pm2.5_cons <- lm(ConsVoteShare ~ pm2.5, data = panelAll)
lm_base_no2_cons <- lm(ConsVoteShare ~ no2, data = panelAll)
lm_base_pm10_cons <- lm(ConsVoteShare ~ pm10, data = panelAll)
lm_base_so2_cons <- lm(ConsVoteShare ~ so2, data = panelAll)
lm_base_oz_cons <- lm(ConsVoteShare ~ oz, data = panelAll)


lm_pm2.5_cons <- lm(ConsVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelAll)
lm_no2_cons <- lm(ConsVoteShare ~ no2 + percHighEdu + medianAge, data = panelAll)
lm_pm10_cons <- lm(ConsVoteShare ~ pm10 + percHighEdu + medianAge, data = panelAll)
lm_so2_cons <- lm(ConsVoteShare ~ so2 + percHighEdu + medianAge, data = panelAll)
lm_oz_cons <- lm(ConsVoteShare ~ oz + percHighEdu + medianAge, data = panelAll)

summary(lm_base_pm2.5_cons)
summary(lm_base_no2_cons)
summary(lm_base_pm10_cons)
summary(lm_base_so2_cons)
summary(lm_base_oz_cons)

summary(lm_pm2.5_cons)
summary(lm_no2_cons)
summary(lm_pm10_cons)
summary(lm_so2_cons)
summary(lm_oz_cons)

#EXPERIMENTAL LM MODEL Labour
lm_base_pm2.5_labour <- lm(LabourVoteShare ~ pm2.5, data = panelAll)
lm_base_no2_labour <- lm(LabourVoteShare ~ no2, data = panelAll)
lm_base_pm10_labour <- lm(LabourVoteShare ~ pm10, data = panelAll)
lm_base_so2_labour <- lm(LabourVoteShare ~ so2, data = panelAll)
lm_base_oz_labour <- lm(LabourVoteShare ~ oz, data = panelAll)

lm_pm2.5_labour <- lm(LabourVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelAll)
lm_no2_labour <- lm(LabourVoteShare ~ no2 + percHighEdu + medianAge, data = panelAll)
lm_pm10_labour <- lm(LabourVoteShare ~ pm10 + percHighEdu + medianAge, data = panelAll)
lm_so2_labour <- lm(LabourVoteShare ~ so2 + percHighEdu + medianAge, data = panelAll)
lm_oz_labour <- lm(LabourVoteShare ~ oz + percHighEdu + medianAge, data = panelAll)

summary(lm_base_pm2.5_labour)
summary(lm_base_no2_labour)
summary(lm_base_pm10_labour)
summary(lm_base_so2_labour)
summary(lm_base_oz_labour)

summary(lm_pm2.5_labour)
summary(lm_no2_labour)
summary(lm_pm10_labour)
summary(lm_so2_labour)
summary(lm_oz_labour)

#EXPERIMENTAL LM MODEL Libdem
lm_base_pm2.5_libdem <- lm(LibdemVoteShare ~ pm2.5, data = panelAll)
lm_base_no2_libdem  <- lm(LibdemVoteShare ~ no2, data = panelAll)
lm_base_pm10_libdem  <- lm(LibdemVoteShare ~ pm10, data = panelAll)
lm_base_so2_libdem <- lm(LibdemVoteShare ~ so2, data = panelAll)
lm_base_oz_libdem <- lm(LibdemVoteShare ~ oz, data = panelAll)

lm_pm2.5_libdem  <- lm(LibdemVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelAll)
lm_no2_libdem  <- lm(LibdemVoteShare ~ no2 + percHighEdu + medianAge, data = panelAll)
lm_pm10_libdem  <- lm(LibdemVoteShare ~ pm10 + percHighEdu + medianAge, data = panelAll)
lm_so2_libdem <- lm(LibdemVoteShare ~ so2 + percHighEdu + medianAge, data = panelAll)
lm_oz_libdem <- lm(LibdemVoteShare ~ oz + percHighEdu + medianAge, data = panelAll)

summary(lm_base_pm2.5_libdem )
summary(lm_base_no2_libdem )
summary(lm_base_pm10_libdem )
summary(lm_base_so2_libdem)
summary(lm_base_oz_libdem)

summary(lm_pm2.5_libdem )
summary(lm_no2_libdem )
summary(lm_pm10_libdem )
summary(lm_so2_libdem)
summary(lm_oz_libdem)

#FIXED EFFECTS MODELS pm2.5 Green 

fe_pm2.5<- plm(GreenVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelDataAll, model = "within",  effect = "twoways")
fe_pm2.5_base <- plm(GreenVoteShare ~ pm2.5, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm2.5)
summary(fe_pm2.5_base)


#FIXED EFFECTS MODELS no2 Green

fe_no2 <- plm(GreenVoteShare ~ no2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_no2_base <- plm(GreenVoteShare ~ no2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_no2)
summary(fe_no2_base)

#FIXED EFFECTS MODELS PM10 Green

fe_pm10 <- plm(GreenVoteShare ~ pm10 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_pm10_base <- plm(GreenVoteShare ~ pm10, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm10)
summary(fe_pm10_base)


#FIXED EFFECTS MODELS SO2 Green

fe_so2 <- plm(GreenVoteShare ~ so2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_so2_base <- plm(GreenVoteShare ~ so2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_so2)
summary(fe_so2_base)


#FIXED EFFECTS MODELS oz Green

fe_oz <- plm(GreenVoteShare ~ oz + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_oz_base <- plm(GreenVoteShare ~ oz, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_oz)
summary(fe_oz_base)


#FIXED EFFECTS MODELS pm2.5 Conservative

fe_pm2.5_cons<- plm(ConsVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelDataAll, model = "within",  effect = "twoways")
fe_pm2.5_base_cons <- plm(ConsVoteShare ~ pm2.5, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm2.5_cons)
summary(fe_pm2.5_base_cons)


#FIXED EFFECTS MODELS no2 Conservative

fe_no2_cons <- plm(ConsVoteShare ~ no2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_no2_base_cons <- plm(ConsVoteShare ~ no2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_no2_cons)
summary(fe_no2_base_cons)

#FIXED EFFECTS MODELS PM10 Conservative

fe_pm10_cons <- plm(ConsVoteShare ~ pm10 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_pm10_base_cons <- plm(ConsVoteShare ~ pm10, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm10_cons)
summary(fe_pm10_base_cons)

#FIXED EFFECTS MODELS SO2 Conservative

fe_so2_cons <- plm(ConsVoteShare ~ so2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_so2_base_cons <- plm(ConsVoteShare ~ so2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_so2_cons)
summary(fe_so2_base_cons)


#FIXED EFFECTS MODELS oz Conservative

fe_oz_cons <- plm(ConsVoteShare ~ oz + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_oz_base_cons <- plm(ConsVoteShare ~ oz, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_oz_cons)
summary(fe_oz_base_cons)


#FIXED EFFECTS MODELS pm2.5 Labour 

fe_pm2.5_labour<- plm(LabourVoteShare ~ pm2.5 + percHighEdu + medianAge, data = panelDataAll, model = "within",  effect = "twoways")
fe_pm2.5_base_labour<- plm(LabourVoteShare ~ pm2.5, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm2.5_labour)
summary(fe_pm2.5_base_labour)


#FIXED EFFECTS MODELS no2 Labour

fe_no2_labour <- plm(LabourVoteShare ~ no2 + percHighEdu+ medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_no2_base_labour <- plm(LabourVoteShare ~ no2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_no2_labour)
summary(fe_no2_base_labour)

#FIXED EFFECTS MODELS PM10 Labour

fe_pm10_labour <- plm(LabourVoteShare ~ pm10 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_pm10_base_labour <- plm(LabourVoteShare ~ pm10, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm10_labour)
summary(fe_pm10_base_labour)

#FIXED EFFECTS MODELS SO2 Labour

fe_so2_labour <- plm(LabourVoteShare ~ so2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_so2_base_labour <- plm(LabourVoteShare ~ so2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_so2_labour)
summary(fe_so2_base_labour)


#FIXED EFFECTS MODELS oz Labour

fe_oz_labour <- plm(LabourVoteShare ~ oz + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_oz_base_labour <- plm(LabourVoteShare ~ oz, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_oz_labour)
summary(fe_oz_base_labour)


#FIXED EFFECTS MODELS pm2.5 Libdem 

fe_pm2.5_libdem<- plm(LibdemVoteShare ~ pm2.5 + percHighEdu+ medianAge, data = panelDataAll, model = "within",  effect = "twoways")
fe_pm2.5_base_libdem <- plm(LibdemVoteShare ~ pm2.5, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm2.5_libdem)
summary(fe_pm2.5_base_libdem)


#FIXED EFFECTS MODELS no2 Libdem

fe_no2_libdem <- plm(LibdemVoteShare ~ no2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_no2_base_libdem <- plm(LibdemVoteShare ~ no2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_no2_libdem)
summary(fe_no2_base_libdem)

#FIXED EFFECTS MODELS PM10 Libdem

fe_pm10_libdem <- plm(LibdemVoteShare ~ pm10 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_pm10_base_libdem <- plm(LibdemVoteShare ~ pm10, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_pm10_libdem)
summary(fe_pm10_base_libdem)

#FIXED EFFECTS MODELS SO2 Libdem

fe_so2_libdem <- plm(LibdemVoteShare ~ so2 + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_so2_base_libdem <- plm(LibdemVoteShare ~ so2, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_so2_libdem)
summary(fe_so2_base_libdem)


#FIXED EFFECTS MODELS oz Libdem

fe_oz_libdem <- plm(LibdemVoteShare ~ oz + percHighEdu + medianAge, data = panelDataAll, model = "within", effect = "twoways")
fe_oz_base_libdem <- plm(LibdemVoteShare ~ oz, data = panelDataAll, model = "within", effect = "twoways")

summary(fe_oz_libdem)
summary(fe_oz_base_libdem)



