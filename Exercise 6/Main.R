set.seed(1948)

a <- 4
i <- 30
samples <- 1000
n <- 30
count <- 0
vector <- numeric(n)

#Simulated value
for (i in 1:samples) {
  vector <- rexp(n, rate = 1/a)
  Y <- sum(vector)
  print(Y)
  if (Y > 90){
    count <- count + 1
  }
}
prop <- count/samples
print(prop)

#Exact Value
# Parâmetros da distribuição gama
k <- 30  # parâmetro de forma
theta <- 1/4  # parâmetro de rate

fiabilidade <- 1 - pgamma(90,rate = theta, shape = 30)
result <- round((fiabilidade - prop) * 100,4)
print(result)

