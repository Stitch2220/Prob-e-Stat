

library(readxl)


electricity <- read_excel("~/Prob-e-Stat/Exercise 3/electricity.xlsx")
View(electricity)

filtered_data <- subset(electricity, electricity[,4] >= 2015)


# Initialize an empty data frame to store the results
proportion_data <- data.frame(Country = character(), Year = integer(), Month = integer(), Proportion = numeric())

# List of countries
countries <- c("Italy", "Latvia", "IEA Total")

# Loop through each country
for (country in countries) {
  # Subset data for the current country
  country_data <- subset(filtered_data, COUNTRY == country)
  
  # Loop through unique years
  for (ano in unique(filtered_data$YEAR)) {
    # Initialize a variable to store the total sum of values for each year
    total_sum_year <- 0
    
    for (mes in unique(filtered_data$MONTH)) {
      # Subset data for the current year and month
      dados <- subset(country_data, YEAR == ano & MONTH == mes)
      
      # Calculate the sum of values for all products except "Renewables"
      total_sum <- sum(as.double(subset(dados, PRODUCT != "Renewables")$VALUE), na.rm = TRUE)
      
      # Calculate the value for "Renewables"
      temp_value <- as.double(subset(dados, PRODUCT == "Renewables")$VALUE)
      
      # Calculate the proportion of renewables
      proportion <- (temp_value / total_sum) * 100
      
      # Create a data frame with the current country, year, month, and proportion
      temp_data <- data.frame(Country = country, Year = ano, Month = mes, Proportion = proportion)
      
      # Combine the current data frame with the existing proportion_data
      proportion_data <- rbind(proportion_data, temp_data)
      
      # Add to the total sum for the current year
      total_sum_year <- total_sum_year + temp_value
    }
    
    # Calculate the mean proportion of renewables for the current year
    mean_proportion <- (total_sum_year / sum(as.double(subset(country_data, YEAR == ano & PRODUCT != "Renewables")$VALUE), na.rm = TRUE)) * 100
    print(paste("Mean proportion of renewables for", country, "in year", ano, ":", mean_proportion))
  }
}
# Create the ggplot
ggplot(proportion_data, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Proportion, color = Country)) +
  
  # Add lines and points for each country
  geom_line() +
  geom_point() +
  
  # Set the y-axis limits and labels
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  
  # Set the x-axis label
  labs(x = "Date", y = "Proportion (%)", title = "Proportion of Renewable Energy Production by Country") +
  
  # Add a legend
  theme_minimal() +
  theme(legend.position = "top")
