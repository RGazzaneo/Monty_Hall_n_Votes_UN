k_monty_hall_choice <- function(choice, switch, k)
{
 # tryCatch(
 #   {
      monty_doors <- rep("goat", k) # creates goat vector
      monty_doors [sample(1:k, 1)] <- "car" # random placement of the car
      monty_opens <- NULL
      available_doors <- c(1:k)
      
      while(length(available_doors) >= 2)
           {
              available_doors<-setdiff(1:k,c(choice,monty_opens))# not chosen doors
              a <-sample(available_doors,1)#choose door among the available doors
              b <- monty_doors[a]
             
              # if chosen door is goat, opens, if not opens the other
              monty_opens <- ifelse(b=="goat", a, sample(1:k,sample(setdiff(available_doors, a),1)))
              
              if (switch==TRUE) # finds the closed door that was not the 1st chosen and replaces it
                  {
                      choice <- sample(available_doors,1)
                  }
          
              k=k-1
            }
    
      #tests if contestant_2nd_choice=car and returns true or false
      return(monty_doors[choice] == "car")
   # },error = function(e) {return(NULL)})
}