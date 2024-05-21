set.seed(2822)

k <- 3.234
n <- 100 #dimensao
m <- 5000 #numero de pares

lambda0 <- 2.90  # Parâmetro sob H0
lambda1 <- 3.15  # Parâmetro sob H1

type_I_errors <- 0
type_II_errors <- 0

for (i in 1:m) {
  sample_H0 <- rpois(n, lambda0)
  mean_H0 <- mean(sample_H0)
  
  sample_H1 <- rpois(n, lambda1)
  mean_H1 <- mean(sample_H1)
  
  if (mean_H0 > k) {
    type_I_errors <- type_I_errors + 1
  }
  
  if (mean_H1 <= k) {
    type_II_errors <- type_II_errors + 1
  }
}
alpha <- type_I_errors / m  # Probabilidade de erro tipo I (Erro tipo I / Total de amostras)
beta <- type_II_errors / m  # Probabilidade de erro tipo II (Erro tipo II / Total de amostras)

# Calcular o quociente entre as probabilidades dos erros
quotient <- beta / alpha
round(quotient,2)
