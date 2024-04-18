
#Exercice 5

set.seed(1950)

#Initialization 
n <- 23
r <- 300
m <- 170
count <- 0
temp <- 0

amostras <- matrix(0, nrow = r, ncol = m)
v <- numeric(r)
vector <- rnorm(n+1)


for (i in 1:r) {
  samples <- numeric(m)
  count <- 0
  for (j in 1:m) {
    vector <- rnorm(n+1)
    T <- sqrt(n) * (vector[1]/sqrt(sum(vector[2:length(vector)]^2)))
    samples[j] <- T
    if (T <= 1.5){
      count <- count + 1
    }
  }
  temp <- count/m
  v[i] <- temp
  amostras[i, ] <- samples
}
media <- mean(v)
print(media)

probabilidade_R <- pt(1.5, df = n)
diferenca <- abs(media - probabilidade_R)

resultado_final <- round(diferenca * 100, 4)
print(resultado_final)
