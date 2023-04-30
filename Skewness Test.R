#skewness

hist(panelData$voteShare)
model <- lm(voteShare ~ pm2.5, data = panel)
summary(model)
plot(model)
model_log <- lm(voteShare ~ pm2.5, data = panel)
plot(model_log)


plot(panel$pm2.5, panel$voteShare)
plot(panel_log$pm2.5, panel_log$voteShare)


library(tidyverse)
library(lmtest)
ggplot(data = panel, aes(x = voteShare)) +
  geom_histogram()

ggplot(data = panel_log, aes(x = voteShare)) +
  geom_histogram()

ggplot(data = panel, aes(y = voteShare)) +
  geom_boxplot()
library(e1071)

skewness(panel$voteShare)
skewness(panel_log$voteShare)


#[1] 9.290738
#positive skew

kurtosis(panel$voteShare)
#[1] 135.2259
#leptokurtic

bptest(lm_model_base_1)
plot(lm_model_base_1)
#no heteroscedasticity

model1<- plm(voteShare ~ pm2.5, data = panelData, model = "within", effect = "twoways")
model2<- plm(voteShare ~ pm2.5+I(pm2.5^2), data = panelData, model = "within", effect = "twoways")

# Add squared term to model
model2 <- update(model1,formula = . ~ . + I(pm2.5^2))
class(model2) <- "plm"
class(model1) <- "plm"

# Perform Wald test
waldtest(model, model2, test = "Chisq")
# Calculate difference in RSS
RSS_1 <- sum(model1$residuals^2)
RSS_2 <- sum(model2$residuals^2)
delta_RSS <- RSS_1 - RSS_2

# Calculate degrees of freedom
df_1 <- df.residual(model1)
df_2 <- df.residual(model1)
delta_df <- df_1 - df_1

# Calculate Wald test statistic and p-value
Wald_stat <- (delta_RSS / delta_df) * ((length(model2$coefficients) - 1) / length(model2$coefficients))
p_val <- 1 - pchisq(Wald_stat, delta_df)

# Print results
cat("Wald test statistic: ", Wald_stat, "\n")
cat("P-value: ", p_val, "\n")
