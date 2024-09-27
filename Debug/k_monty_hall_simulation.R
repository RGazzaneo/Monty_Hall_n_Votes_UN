k_monty_hall_simulation <- function(n, k)
{
  choice <- sample(1:k, n, TRUE)
  with_switch <- c()
  without_switch <- c()

  for (i in choice)
  {
    a <- NULL
    while(is.null(a))
    {a <- k_monty_hall_choice(i, TRUE,k)}
    with_switch <- c(with_switch, a)
    
    b <- NULL
    while(is.null(b))
    {b <- k_monty_hall_choice(i, FALSE,k)}
    without_switch <- c(without_switch, b)
  }

  results <- data.frame(choice,with_switch,without_switch)
  
  #proportion of win w/switch
  p_with = (sum(results$with_switch)/n)
  #proportion of win w/out switch
  p_without = (sum(results$without_switch)/n)
  
  return(c(p_with,p_without))
}

results<- k_monty_hall_simulation(10, 10)

cat("ANSWER:\n Proportion of wins with switch:", results[1]*100, "%\n Proportion of wins without switch:", results[2]*100, "%" )