monty_hall_simulation <- function(n)
{
  choice1 <- sample(1:3, n, TRUE)
  with_switch <- c()
  without_switch <- c()
  
  for (i in choice1)
  {
    with_switch <- c(with_switch, monty_hall_choice(i, switch = TRUE))
    without_switch <- c(without_switch, monty_hall_choice(i, switch = FALSE))
  }
  
  results <- data.frame(choice1,with_switch,without_switch)
  
  #proportion of win w/switch
  p_with = (sum(results$with_switch)/n)
  #proportion of win w/out switch
  p_without = (sum(results$without_switch)/n)
  
  return(c(p_with,p_without))
}

cat("ANSWER:\n Proportion of wins with switch:", results[1], "%\n Proportion of wins without switch:", results[2], "%" )