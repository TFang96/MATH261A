library(ggplot2)
library(dplyr)
library(knitr)


## Read in the CSV
compensation <- read.csv("Employee_Compensation_20250829.csv")
#remove na
compensation <- na.omit(compensation)
#drop rows with missing values
compensation <- compensation[complete.cases(compensation[, c("Total.Benefits", "Total.Compensation")]), ]
#deal with commas (e.g. 1,000 = 1000)
compensation$Total.Benefits <- as.numeric(gsub(",", "", compensation$Total.Benefits))
compensation$Total.Salary <- as.numeric(gsub(",", "", compensation$Total.Salary))

## build regression model for total compensation as the predictor variable for total benefits
model <- lm(Total.Benefits ~ Total.Salary, data=compensation)

#print out a summary of the regression model (e.g. estimated slope, p-value, etc.)
summary(model)

#plot the data
plot(compensation$Total.Salary, compensation$Total.Benefits, xlab = "Total Compensation", ylab = "Total Benefits", 
     main = "Total Benefits vs. Total Compensation")

#overlay regression line
abline(model, col = "red", lwd=2)

## print data summary
compensation %>%
  summarise(
    Mean_Salary   = mean(Total.Salary, na.rm = TRUE),
    Salary_SD     = sd(Total.Salary, na.rm = TRUE),
    Min_Salary    = min(Total.Salary, na.rm = TRUE),
    Max_Salary    = max(Total.Salary, na.rm = TRUE),
    Mean_Benefits = mean(Total.Benefits, na.rm = TRUE),
    Benefits_SD   = sd(Total.Benefits, na.rm = TRUE),
    Min_Benefits  = min(Total.Benefits, na.rm = TRUE),
    Max_Benefits  = max(Total.Benefits, na.rm = TRUE)
  ) %>%
  t() %>%
  kable(col.names = c("Value"))
