test_monty_hall <- function(n)
{
  choice1 <- sample(1:3, n, TRUE) #creates random vector w/ chosen door for the n tests
  with_switch <- c() #creates empty vector to summarize n tests w/ switch
  without_switch <- c() #creates empty vector to summarize n tests w/out switch
  
  for (i in choice1) #run function n times w/ and w/out switch
  {
    with_switch <- c(with_switch, monty_hall_choice(i, switch = TRUE))
    without_switch <- c(without_switch, monty_hall_choice(i, switch = FALSE))
  }
  
  #summarize results in a table
  return(results <- data.frame(choice1,with_switch,without_switch))
}

print('ANSWER:')
print(results_summary <- test_monty_hall(100))
