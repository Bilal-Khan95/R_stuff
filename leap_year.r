leap_tear <- function(year){
  if(year%%4 == 0){
    print("This is a leap year")
  }else{
    print("This is not a leap year.")
  }
}

year_number <- as.integer(readline(prompt = "What year is it?!\n"))

leap_tear(year_number)