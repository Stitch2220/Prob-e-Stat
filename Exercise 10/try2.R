# Install and load EnvStats package
library(EnvStats)

# Dados observados
dados <- c(6.52, 5.48, 7.01, 7.07, 9.76, 7.45, 10.11, 8.78, 8.53, 7.14, 8.56, 8.22, 
           8.11, 8.72, 9.89, 12.01, 9.14, 7.26, 11.05, 12.14, 8.44, 10.98, 6.95, 11.37, 
           8.65, 11.03, 10.55, 8.77, 10.04, 9.70, 9.43, 8.91, 10.79, 12.38, 7.41, 10.80, 
           8.40, 8.16, 9.22, 11.29, 8.54, 12.57, 8.53, 9.12, 8.53, 8.62, 12.03, 7.64, 
           11.13, 7.18, 8.16, 9.29, 6.89, 10.45, 11.20, 9.38, 8.79, 4.71, 9.35, 10.80, 
           10.90, 9.86, 9.50, 8.17, 11.19, 7.89, 8.72, 8.74, 8.87, 8.63, 9.28, 6.58, 10.74, 
           8.12, 6.92, 6.89, 5.19, 10.73, 10.38, 8.74, 8.96, 8.60, 4.92, 7.48, 8.96, 10.96, 
           8.08, 7.15, 8.82, 5.69, 8.09, 10.21, 5.80, 9.76, 11.90, 8.73, 7.10, 11.13, 5.07, 
           9.43, 8.04, 10.76, 7.95, 9.06, 9.39, 7.70, 5.92, 9.32, 10.53, 10.26, 6.31, 6.34, 
           5.36, 7.75, 7.74, 8.42, 10.76, 11.39, 8.42, 9.79, 9.45, 9.17, 7.91, 5.79, 5.70, 
           8.74, 10.33, 11.00, 6.17, 9.11, 9.57, 11.82, 10.35, 8.37, 6.66, 6.97, 9.66, 12.18, 
           10.80, 11.14)

# Definindo as classes
a <- 4.5
b <- 13
n_classes <- 6
classes <- cut(dados, breaks = seq(a, b, length.out = n_classes + 1))

# Calculando as frequências observadas para cada classe
freq_observadas <- table(classes)

# Calculando as probabilidades esperadas para cada classe (distribuição triangular)
prob_triangular <- diff(ptri(q = seq(a, b, length.out = n_classes + 1), min = a, max = b, mode = (a + b) / 2))

# Executando o teste de ajustamento do qui-quadrado
test_result <- chisq.test(x = freq_observadas, p = prob_triangular)

# Valor-p arredondado a 4 casas decimais
p_value <- round(test_result$p.value, 4)

# Imprimindo o valor-p do teste do qui-quadrado
print(p_value)
