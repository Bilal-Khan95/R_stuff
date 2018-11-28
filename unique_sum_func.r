unique_sum <- function(a,b,c){
  if(a==b & a==c){
    print("NO UNIQUE NUMBERS.", 0)
  }else if(a == b){
    print("c is ", c)
  }else if(b == c){
    print("A is ", a)
  }else if(a == c){
    print("B is ", b)
  }else{
    print(c("A + B + C = ", a+b+c))
  }
}

unique_sum(1,5,13)
