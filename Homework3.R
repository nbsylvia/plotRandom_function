GuessTheNumber <- function(lower = 0, upper = 10, seed = NULL) {
  if (!is.numeric(lower) || !is.numeric(upper)) {
    stop("lower and upper must be numeric values")
  }
  if (lower >= upper) {
    stop("lower must be less than upper")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # generate the random number
  random_number <- sample(lower:upper, 1)

  number_of_guesses <- 0
  guesses <- vector()
  guess <- NA
  signal <- 0
  
  # tell user to try again
  while (signal ==0 ) {
    guess <- readline("Guess the number between %d and %d: " )
    guess <- as.numeric(guess)
    
    # if (is.na(guess)) {
    #   cat("Invalid input. Please enter an integer between %d and %d.\n" % lower % upper)
    #   next
    # }
    # 
    number_of_guesses <- number_of_guesses + 1
    guesses[number_of_guesses] <- guess
    
    if(guess == random_number){
      cat("You guess it correctly!! \n")
      signal <- 1
    }else if (guess < random_number) {
      cat("Too low! Guess again.\n")
    } else if (guess > random_number) {
      cat("Too high! Guess again.\n")
    }
  }
  
  # create list of results to return
  results <- list(
    RandomNumber = random_number,
    NumGuesses = number_of_guesses,
    Guesses = guesses
  )
  
  return(results)
}
