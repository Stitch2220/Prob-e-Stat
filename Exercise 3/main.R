

library(readxl)


electricity <- read_excel("C:/Users/Catarina/Downloads/electricity.xlsx")
View(electricity)

filtered_data <- subset(electricity,electricity[,7] == "Renewables" & electricity[,4] >= 2015)

x2 <- subset(electricity,YEAR == 2015 & MONTH == 7)

freq_table <- table(x2[,7])

prop_table <- prop.table(freq_table)
proportion <- prop_table["Renewables"]


