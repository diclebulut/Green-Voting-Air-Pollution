manifesto <- read.csv("MPDataset_MPDS2023a.csv")

uk <- subset(manifesto, manifesto$countryname == "United Kingdom")

greenPolicies <- data.frame(edate = uk$edate, date = uk$date, party = uk$partyname, anti_growth = uk$per416, environment = uk$per501, sustain = uk$per416_2)

greenPolicies2010 <- greenPolicies[greenPolicies$date >= 201005, ]

mean_green <- mean(subset(greenPolicies2010, party == "Green Party of England and Wales")$environment)

# calculate the mean of the 'value' column for rows with category 'B'
mean_non_green <- mean(subset(greenPolicies2010, party != "Green Party of England and Wales")$environment)


mean_green_2015 <- mean(subset(greenPolicies2010, party == "Green Party of England and Wales" & date == "201505" )$environment)
mean_non_green_2015 <- mean(subset(greenPolicies2010, party != "Green Party of England and Wales" & date == "201505" )$environment)

mean_green_2017 <- mean(subset(greenPolicies2010, party == "Green Party of England and Wales" & date == "201706" )$environment)
mean_non_green_2017 <- mean(subset(greenPolicies2010, party != "Green Party of England and Wales" & date == "201706" )$environment)


mean_green_2019 <- mean(subset(greenPolicies2010, party == "Green Party of England and Wales" & date == "201912" )$environment)
mean_non_green_2019 <- mean(subset(greenPolicies2010, party != "Green Party of England and Wales" & date == "201912" )$environment)


greenManifesto <- data.frame(year = rep(c(2015, 2017, 2019, "Overall"),2),
                            party = rep(c("Green Party of England and Wales", "Other Parties"), each = 4),
                            environment = c(mean_green_2015, mean_green_2017, mean_green_2019, mean_green, mean_non_green_2015, mean_non_green_2017, mean_non_green_2019, mean_non_green))
ggplot(greenManifesto, aes(x = year, y = environment, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Inclusion of Envrionmental Protection in Election Manifesto ", x = "Year", y = "Score") +
  scale_fill_manual(values = c("Green", "Brown"), name = "Parties")

library(ggplot2)
