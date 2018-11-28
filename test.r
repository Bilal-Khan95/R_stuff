sample <- function(a,b){
  if(a==0){
    print(b)
  }else{
  print(a+b)
  }
}
print("Enter 2 numbers")
number_1 <- as.integer(readline())
number_2 <- as.integer(readline())
result <- sample(number_1, number_2)

for(i in 1:9){
  print(result)
}