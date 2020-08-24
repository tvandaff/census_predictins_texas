install.packages("tidyverse")
library(tidyverse)
library(dplyr)

#Load Data Set
pop_est <- read.csv("~/nst-est2018-alldata.csv", header=TRUE)

#Filter data for state
texas <- filter(pop_est, NAME == 'Texas')

#Filter out excess rows
estimates <- select(texas, "POPESTIMATE2010":"POPESTIMATE2018")

#Remove column names 
names(estimates) <- NULL

#Turn values into a numeric vector 
estimates <- as.numeric(estimates)

#Define population estimate years as a vector 
est_years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

#Combine estimates and est_years vectors into a dataframe 
(df <- data.frame(population = estimates, year = est_years))

#Plot the data with a scatter in ggplot
ggplot(df, aes(x = year, y = population)) +
  geom_point()

#Let's see if a linear regression fits the data
ggplot(df, aes(x = year, y = population)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "red")

#Define the Linear Regression Model for this data 
reg <- lm(population ~ year, df)

#Summarize the Linear MOdel Characteristics
summary(reg)

#Predict into the future
predict(reg, newdata=data.frame(year = c(2019, 2020,
                                         2021, 2022, 
                                         2023, 2024)))
