temp_gauge <- function(temp, is_summer){
  if(is_summer == T){
    if(temp>60 & temp <=100){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  if(is_summer == F){
    if(temp>60 & temp <=90){
      return(TRUE)
    }else{
      return(F)
    }
  }
}
print("What is the temperature?")

temp_number <- as.integer(readline())

temp_gauge(temp_number, T)
