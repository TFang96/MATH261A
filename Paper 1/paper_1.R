library(ggplot2)


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
