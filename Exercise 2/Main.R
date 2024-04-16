
#EXERCISE 2


# Load libraries 
library(readr)
library(ggplot2)

# Read the CSV file
data <- read_csv("~/Prob-e-Stat/Exercise 2/master.csv")

# Filter the data
filtered_data <- subset(data, year == "2002" & age == "55-74 years")

# Create the side-by-side box plots without the legend
ggplot(filtered_data, aes(x = sex, y = `suicides/100k pop`, fill = sex)) +
  geom_boxplot() +
  labs(title = "Comparison of Suicides per 100k by Sex in 2002 (Age Group: 55-74 years)", 
       x = "Sex", 
       y = "Suicides per 100k") +
  theme_minimal()


