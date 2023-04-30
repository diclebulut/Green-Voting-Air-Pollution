#Correlation heatmap 
# Load required libraries
install.packages('corrplot')
library(corrplot)

# Select columns
independent_cols <- c('pm2.5', 'no2', 'pm10', 'percHighEdu', 'medianIncome')
independent_data <- panel[, independent_cols]

# Compute correlation matrix
corr <- cor(independent_data)

# Create heatmap
par(mar = c(4, 4, 2, 2)) # Adjust margin sizes
corrplot(corr, method = 'color', type = 'upper', tl.col = 'black', tl.srt = 45, 
         diag = FALSE, addrect = 2, col = colorRampPalette(c('blue', 'white', 'red'))(50), 
         tl.cex = 0.8, cl.cex = 0.8)

# Add annotations
for (i in 1:nrow(corr)) {
  for (j in 1:ncol(corr)) {
    text(j, i, format(round(corr[i, j], 2), nsmall = 2), 
         col = ifelse(corr[i, j] > 0.5, 'black', 'white'))
  }
}

# Show plot
#correlation heatmap end

#removing skew (outliers)
# Load required packages
library(plm)
library(lmtest)
install.packages('betareg')
library(betareg)
library(e1071)


panelData <- pdata.frame(panel, index = c("id", "Year"))
noSkew <- panelData[panelData$voteShare <= 20, ]

#VISUALISATIONS
# Step 1: Estimate your fixed effects model
fe_pm10 <- plm(voteShare ~ pm10 + percHighEdu + medianIncome, data = panelData, model = "within", effect = "twoways")
fe_pm10_base <- plm(voteShare ~ pm10, data = panelData, model = "within", effect = "twoways")

residuals <- residuals(fe_pm10_base)
skewness <- skewness(residuals)
cat("Skewness:", skewness, "\n")

# QQ plot of residuals
qqnorm(residuals, main="QQ Plot of Residuals")
qqline(residuals)

# Plot histogram of residuals
hist(residuals, main="Histogram of Residuals", xlab="Residuals")


#SERIAL CORRELATION TEST
# Step 2: Obtain residuals
residuals <- resid(fe_pm10_base)
residuals
n <- length(residuals) 
numerator <- sum(diff(residuals[-1])^2) # Exclude the first observation from residuals
denominator <- sum(residuals^2)
durbin_watson <- numerator / denominator

# Step 4: Interpret results
cat("Durbin-Watson statistic:", durbin_watson, "\n")

if (durbin_watson < 2) {
  cat("Positive serial correlation is detected (Durbin-Watson < 2).")
} else if (durbin_watson > 2) {
  cat("Negative serial correlation is detected (Durbin-Watson > 2).")
} else {
  cat("No significant evidence of serial correlation (Durbin-Watson = 2).")
}

#END SERIAL CORRELATION TEST


#robust standard errors (NOT COMPLETE)
robust_se <- sqrt(diag(vcovHC(fe_pm10_base, cluster = "group", type = "HC1"))) # HC1 for heteroscedasticity-consistent (HC) standard errors, cluster by individual

coefficients <- coef(fe_pm10_base)

# Compute t-values and p-values using coeftest()
tvals_with_pvals <- coeftest(fe_pm10_base, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1"))[, c("t value", "Pr(>|t|)")]

# Combine coefficients, robust standard errors, t-values, and p-values
se_with_tval_pval <- cbind(robust_se, tvals_with_pvals)
se_with_tval_pval
colnames(se_with_tval_pval) <- c("Robust SE", "t-value", "p-value")
print(cbind(coefficients, se_with_tval_pval))



#heterodescasity test with scale location plot
library(car)
library(ggplot2)

#Extract the residuals from the fixed effects model
residuals <- resid(fe_model_pm2.5_base)

# Extract the fitted values from the fixed effects model
fitted_values <- fitted(fe_model_pm2.5_base)

# Create a data frame with residuals and fitted values
data <- data.frame(residuals = residuals, fitted_values = fitted_values)

# Create a scale-location plot using ggplot2
scale_location_plot <- ggplot(data, aes(x = fitted_values, y = sqrt(abs(residuals)))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "solid") +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "Square Root of Standardized Residuals") +
  theme_minimal()

# Add a horizontal line at y = 0 to represent zero residual mean
scale_location_plot <- scale_location_plot + geom_hline(yintercept = 0, linetype = "dashed")

# Show the scale-location plot
print(scale_location_plot)
#heterodescasity test with scale location plot END


#Check the zero conditional mean assumption
#IMPORTANT
residuals <- residuals(fe_model_pm2.5_base)

# Create a vector of row numbers
row_numbers <- 1:length(residuals)

# Plot residuals against row numbers
plot(row_numbers, residuals, xlab = "Row Number", ylab = "Residuals", main = "Residual Plot for Fixed Effects Model")

#END the zero conditional mean assumption

#Use VIF test for multicollinearity

vif(lm_pm10)
#The general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity requiring correction.

