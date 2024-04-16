

library(readxl)


electricity <- read_excel("~/Prob-e-Stat/Exercise 3/electricity.xlsx")
View(electricity)

filtered_data <- subset(electricity, electricity[,4] >= 2015)

x2 <- subset(electricity,YEAR == 2015 & MONTH == 7)

proportion_data <- data.frame(Year = integer(), Month = integer(), Proportion = numeric())


for (ano in unique(filtered_data$YEAR)) {
  for (mes in unique(filtered_data$MONTH)) {
    dados <- subset(filtered_data, filtered_data$YEAR == ano & filtered_data$MONTH == mes)
    freq_table <- table(dados[,7])
    prop_table <- prop.table(freq_table)
    proportion <- prop_table["Renewables"]
    temp_data <- data.frame(Year = ano, Month = mes, Proportion = proportion)
    proportion_data <- rbind(proportion_data, temp_data)
    
  }
  
}


prop_table <- prop.table(freq_table)
proportion <- prop_table["Renewables"]


