
#EXERCISE 1


# Load libraries 
library(readr)
library(ggplot2)
library(datasets)


# Import "Paises_PIB_ICH.csv" with readr library
data <- read_csv("~/Prob-e-Stat/Exercise 1/Paises_PIB_ICH.csv")
View(data)
filtered_data <- subset(data, data[,2] == "Africa" | data[,2] == "Asia")


GDP <- filtered_data[,3]
HCI <- filtered_data[,4]
specific_shapes <- c("United Arab Emirates" = 2, "Nepal" = 3, "Comoros" = 4, "Namibia" = 5)
specific_labels <- c("United Arab Emirates" = "UAE", "Nepal" = "Nepal", "Comoros" = "Comoros", "Namibia" = "Namibia")
filtered_data$Countries <- ifelse(filtered_data$Country %in% names(specific_shapes), as.character(filtered_data$Country), "Other")

#Plot the graph 
ggplot(filtered_data, aes(x = GDP, y = HCI, color = Continent, shape = Countries)) +
  geom_point(size = 2) +
  geom_text(aes(label = ifelse(Country %in% names(specific_labels), specific_labels[as.character(Country)], "")), 
    vjust = -1, hjust = 1, size = 4) +  # Add text labels near specific dots
  scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
  scale_color_manual(values = c("Africa" = "#bc6c25", "Asia" = "#669bbc")) +
  scale_shape_manual(values = c("United Arab Emirates" = 6, "Nepal" = 3, "Comoros" = 9, "Namibia" = 4, "Other" = 19)) +
  labs(title = "Scatter Plot of HCI in function of GDP", 
    x = "GDP per capita (log10 scale)", 
    y = "HCI - Human Capital Index",
    color = "Continent") +
  theme_minimal()

                          