##### Let's Make a Deal! #####
### 1 ###
# Creating a new class: `door`,
# which takes on a value in c(1:3)
# depending on the `choice` of the player.
door <- list(choice = c(1:3))
class(door) <- "door"

### 2 ###
# Creating the `PlayGame` generic, preparing it to take `door`
# as an input
PlayGame <- function(door){
  UseMethod("PlayGame")
}

# And the function itself
PlayGame.Door <- function(choice){
  # Simulating which door conceals the car
  # It can be a number between 1 and 3 with equal
  # probability
  cardoor <- as.numeric(sample(c(1:3), size = 1))
  
  if(player_choice == cardoor){
    # If the choice matches correctly, the player wins
    return("Winner winner, chicken dinner! It's a match!")
  } else{
    # It's a coarse way to express sympathy, I guess
    return("What a fuckin' loser! No match!")
  }
}

### 3 ###
# The construction function for the new door
new_door <- function(choice){
  output <- list(door = choice)
  class(output) <- "door"
  return(output)
}

# And a validator, ensuring that `choice`
# is actually a number 1, 2, or 3
validate_door <- function(choice){
  # Testing that the choice is actually a number
  if(! is.numeric(choice)){
    stop("Gotta choose a number, buddy!")
  }
  
  # Testing if that number is a valid choice
  if(choice %!in% c(1:3)){
    stop("Gotta choose something between 1 and 3, buddy!")
  }
}

# Thus, re-constructing the game with the constructor
# and validator
PlayGame.Door <- function(choice){
  # The validator
  validate_door(choice)
  
  # and the constructor
  player_choice <- new_door(choice)
  
  
  # Simulating which door conceals the car
  # It can be a number between 1 and 3 with equal
  # probability
  cardoor <- as.numeric(sample(c(1:3), size = 1))
  
  if(player_choice == cardoor){
    # If the choice matches correctly, the player wins
    return("Winner winner, chicken dinner! It's a match!")
  } else{
    # It's a coarse way to express sympathy, I guess
    return("What a fuckin' loser! No match!")
  }
}


# Testing it!
set.seed(666)
PlayGame.Door(choice = "cat")
PlayGame.Door(choice = 0)
PlayGame.Door(choice = 1)

