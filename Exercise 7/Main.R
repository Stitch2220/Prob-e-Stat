library(stats4)

data <- c(8.54,4.76,5.15,4.96,6.25,7.22,12.9,6.04,8.86,4.88,6.54,4.53,4.7,5.38,5.96,5.17,5.09,5.11)

nll <- function(theta) {
  -sum(log(theta * data^(-theta-1) * 4.5^theta))
}

result <- mle(nll, start = list(theta = 3.4))
theta_MLE <- result@coef["theta"]
p <- 0.25
a <- 4.5
x_p <- a * (-log(1 - p))^(1/theta_MLE)

# Verdadeiro valor do quantil com theta=3.4
x_p_true <- a * (-log(1 - p))^(1/3.4)


desvio_absoluto <- abs(x_p_true - x_p)
# Imprimir o resultado
print(paste("Desvio absoluto entre a estimativa e o valor verdadeiro do quantil:", round(desvio_absoluto, 4)))
