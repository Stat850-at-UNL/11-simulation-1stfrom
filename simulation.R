# Function to simulate a single roll of two dice
roll_dice <- function() {
  return(sum(sample(1:6, 2, replace = TRUE)))
}

# Function to play a game of Craps one step at a time
play_craps <- function() {
  cat("Starting a new game of Craps...\n")
  
  # Initial roll (come-out roll)
  point <- roll_dice()
  cat("You rolled a", point, "\n")
  
  # Check the result of the come-out roll
  if (point %in% c(7, 11)) {
    cat("Natural! You win!\n")
    return("win")
  } else if (point %in% c(2, 3, 12)) {
    cat("Craps! You lose.\n")
    return("lose")
  } else {
    cat("Point established at", point, ". Now you need to roll", point, "again before a 7 to win.\n")
    
    # Roll until the player either makes the point or rolls a 7
    while (TRUE) {
      # Ask the player if they want to roll again
      continue <- readline(prompt = "Press Enter to roll again, or type 'exit' to quit: ")
      if (tolower(continue) == "exit") {
        cat("Game ended. Thank you for playing!\n")
        return("exit")
      }
      
      # Roll the dice again
      roll <- roll_dice()
      cat("You rolled a", roll, "\n")
      
      if (roll == point) {
        cat("You made your point! You win!\n")
        return("win")
      } else if (roll == 7) {
        cat("You rolled a 7. You lose.\n")
        return("lose")
      } else {
        cat("Rolling again...\n")
      }
    }
  }
}

# Function to play multiple games of Craps interactively
play_multiple_games <- function() {
  while (TRUE) {
    result <- play_craps()
    
    if (result == "exit") break
    
    # Ask if the player wants to play again
    play_again <- readline(prompt = "Do you want to play another game? (y/n): ")
    if (tolower(play_again) != "y") {
      cat("Thanks for playing!\n")
      break
    }
  }
}

# Start the game
play_multiple_games()