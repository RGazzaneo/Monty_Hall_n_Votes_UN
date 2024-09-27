monty_hall_choice <- function(choice1, switch = TRUE)
{
  monty_doors <- sample(c("goat", "goat", "car"))  # random placement of the car and goats
  available_doors <- setdiff(1:3, choice1) # not chosen doors
  
  #choose door among the 2 available doors
  a <-sample(available_doors,1)
  b <- monty_doors[a]
  # if chosen door is goat, opens, if not opens the other
  monty_opens <- ifelse(b=="goat", a, setdiff(1:3,c(choice1,a)))

  # If contestant switches, they choose the other unopened door
  if (switch) # finds the closed door that was not the 1st chosen and replaces it
  {
    free_door <- setdiff(1:3, c(choice1, monty_opens))
    choice2 <- free_door
  }
  else {choice2 <- choice1}
  
  # tests if contestant_2nd_choice=car and returns true or false  
  return(monty_doors[choice2] == "car")
}

