roll_dice <- function() {
  # Input: None
  # Output: an integer from 1 to 12
  # Description: Generate 2 random integers from 1 to 6 and sum them
  die1 <- sample(1:6, 1)
  die2 <- sample(1:6, 1)
  return(die1 + die2)
}

come_out_outcome <- function(roll) {
  # Input: roll, integer from 2 to 12
  # Output: A string indicating "win", "lose", or "continue"
  # Description: Handles the outcome of the come-out roll  
  if (roll %in% c(7, 11)) {
    return("win")
  } else if (roll %in% c(2, 3, 12)) {
    return("loss")
  } else { 
    return("point")
  }
}

point_phase <- function(point) {
  # Input: point, integer representing the point number
  # Output: A string indicating "win" or "lose"
  # Description: Simulates rolling to match the point to win or hit a 7 to lose
  repeat {
    roll <- roll_dice()
    if (roll == point) {
      return("win")
    } else if (roll == 7) {
      return("loss")
    }
  }
}

simulate_craps_game <- function(game_id) {
  # Input: game_id - Integer representing the game identifier
  # Output: Data frame with columns game_id, roll_id, roll_result, outcome, point
  # Description: Simulates a single game of Craps and records all relevant details
  
  game_log <- data.frame(
    game_id = integer(),
    roll_id = integer(),
    roll_result = integer(),
    outcome = character(),
    point = integer(),
    stringsAsFactors = FALSE
  )
  
  roll_id <- 1
  first_roll <- roll_dice()
  outcome <- come_out_outcome(first_roll)
  point <- ifelse(outcome == "point", first_roll, NA)
  
  game_log <- rbind(game_log, data.frame(
    game_id = game_id,
    roll_id = roll_id,
    roll_result = first_roll,
    outcome = outcome,
    point = point
  ))
  
  if (outcome == "point") {
    repeat {
      roll_id <- roll_id + 1
      new_roll <- roll_dice()
      outcome <- ifelse(new_roll == point, "win", ifelse(new_roll == 7, "loss", "continue"))
      game_log <- rbind(game_log, data.frame(
        game_id = game_id,
        roll_id = roll_id,
        roll_result = new_roll,
        outcome = outcome,
        point = point
      ))
      if (outcome %in% c("win", "loss")) {
        break
      }
    }
  }
  
  return(game_log)
}

summarize_craps_game <- function(game_log) {
  # Input: Data frame from simulate_craps_game
  # Output: Data frame with summary: game_id, n_rolls, final_outcome, point
  # Description: Summarizes the results of a single game
  
  game_id <- game_log$game_id[1]
  n_rolls <- nrow(game_log)
  final_outcome <- tail(game_log$outcome, 1)
  point <- ifelse(game_log$outcome[1] == "point", game_log$point[1], NA)
  
  summary <- data.frame(
    game_id = game_id,
    n_rolls = n_rolls,
    final_outcome = final_outcome,
    point = point,
    stringsAsFactors = FALSE
  )
  return(summary)
}

run_craps_simulation <- function(N) {
  # Input: an integer N determining the number of games to simulate
  # Output: Data frame summarizing all games: game_id, n_rolls, final_outcome, point
  # Description: Runs N simulations and summarizes each game
  
  summary_results <- data.frame(
    game_id = integer(),
    n_rolls = integer(),
    final_outcome = character(),
    point = integer(),
    stringsAsFactors = FALSE
  )
  
  for (game_id in 1:N) {
    game_log <- simulate_craps_game(game_id)
    game_summary <- summarize_craps_game(game_log)
    summary_results <- rbind(summary_results, game_summary)
  }
  
  return(summary_results)
}