set.seed(1592)

n <- 12
distancias <- c(31.8,31.7,35.2,37.1,31.7,36.1,36.3,33.2,34.3,37.5,30.4,34.6,32.4,31.7,30.2,34.3,35.6,34.9,38.9)

sampled_observations <- sample(distancias,n,replace = FALSE)
#----------------------------------------------------------------

gamma <- 0.96
alpha_a <- (1 - gamma) / 2
alpha_b <- (1 + gamma) / 2

# Quantis da distribuição qui-quadrado
a <- qchisq(alpha_a, df = n - 1, lower.tail = TRUE)
b <- qchisq(alpha_b, df = n - 1, lower.tail = TRUE)

# Calcular variância amostral
s2 <- var(sampled_observations)

# Limites do intervalo de confiança
IC_lower <- (n - 1) * s2 / b
IC_upper <- (n - 1) * s2 / a

# Mostrar o intervalo de confiança
cat("Intervalo de confiança para σ²: [", IC_lower, ", ", IC_upper, "]\n")

#----------------------------------------------------------------

# Definir as equações a serem resolvidas
equations <- function(x) {
  c <- x[1]
  d <- x[2]
  
  eq1 <- pchisq(d, df = n - 1) - pchisq(c, df = n - 1) - gamma
  eq2 <- dchisq(d, df = n + 3) - dchisq(c, df = n + 3)
  
  return(c(eq1, eq2))
}

# Valores iniciais para (a, b)
initial_values <- c(a, b)

# Resolver o sistema de equações
solution <- fsolve(equations, initial_values)

# Valores de (c, d)
c <- solution$x[1]
d <- solution$x[2]

# Mostrar os valores de (c, d)
cat("Valores de (c, d): (", c, ", ", d, ")\n")

# Novo intervalo de confiança com (c, d)
IC_new_lower <- (n - 1) * s2 / d
IC_new_upper <- (n - 1) * s2 / c

# Mostrar o novo intervalo de confiança
cat("Novo intervalo de confiança para σ²: [", IC_new_lower, ", ", IC_new_upper, "]\n")

#----------------------------------------------------------------

# Calcular e mostrar a diferença de amplitudes
old_amplitude <- IC_upper - IC_lower
new_amplitude <- IC_new_upper - IC_new_lower
amplitude_difference <- old_amplitude - new_amplitude

cat("Diferença de amplitude (três casas decimais):", round(amplitude_difference, 3), "\n")

