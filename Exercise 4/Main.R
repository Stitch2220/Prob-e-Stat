set.seed(2255)
i <- c(1,2,3,4,5,6,7,8,9,10)
teste <- 1:10/55
n_simulations <- 150

count <- 0
desligado <- 0
for (iteracao in 1:n_simulations) {
  vector <- sample(1:10,9,replace = TRUE, prob = teste)
  
  if (1 %in% vector){
    next
  }
  
  if (!(1 %in% vector)){
    desligado <- desligado + 1
  }
  
  if (2 %in% vector){
    count <- count + 1
  }
  
}
print(count/desligado)

