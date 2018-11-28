blackjack <- function(a,b){
  if(a <= 21 & b <=21){
    if(a>b){
      print(a)
    }else{
      print(b)
    }
  }else{
    print(0)
  }
}

n_1 <- as.integer(readline())
n_2 <- as.integer(readline())

blackjack(n_1, n_2)