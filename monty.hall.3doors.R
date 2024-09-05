#' @title
#'   Step 1: Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#' create a game 
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#' Step 2: Contestant Selects a Door
#' @description
#' The contestant chooses their first door
#' @details
#' Need to write a function to select a random door
#' @param 
#' no arguments are used
#' @return 
#' The function returns one random door chosen by the contestant
#' @examples
#' select a door 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Step 3: Host Opens Goat Door
#' @description
#' The host will open a door with a goat behind it. 
#' @details
#' The host will always choose a door with a goat behind it but it cannot be the door that the contestant already chose. 
#' @param 
#' No arguments are used
#' @return 
#' a door that has a goat behind it 
#' @examples
#' open a goat door 
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' Step 4: Change Doors
#' @description
#' The contestant chooses to either switch their door option or keep their original door 
#' @details
#' The contestant will then have a 66% of choosing a door with a car behind it. 
#' @param 
#' No arguments are used
#' @return 
#' The door that the contestant chooses, either the same door or a new door
#' @examples
#' change door 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine If the Contestant Has Won
#' @description
#' By analyzing the contestant's choices we will see if they chose the car or the goat
#' @details
#' We will see if the contestant chose the door with the car behind it or the goat door by seeing if they switched or kept their original door choice
#' @param 
#' No arguments are used
#' @return 
#' If the contestant has won
#' @examples
#' Determine winner 
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#' test the game
#' @description
#' play the game
#' @details
#' Test the game from start to finish to see if the contestant has won
#' @param 
#' no arguments used
#' @return
#' overall outcome of game
#' @examples
#' game outcome
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Document results by testing game 
#' @description
#' Document number of times the contestant wins or loses
#' @details
#' Create a table in order to save the outcome of contestant's choices and seeing how many times they win versus lose
#' @param 
#' No arguments used
#' @return 
#' Table of results
#' @examples
#' results in a table  
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}