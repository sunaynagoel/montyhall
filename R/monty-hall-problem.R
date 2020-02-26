#' @title
#'   Create a new Monty Hall Problem game.
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
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'
#'  Contestant selects a door out of three available.
#'
#' @description
#'
#'  `select_door()` allows the user to makes a choice of a door
#'  "1,2, or 3"
#'
#' @details
#'
#'  The select_door() let users pick a door. In this function, one of the door
#'  out of three is selected at random and the choice is stored in a vector.
#'  At this point the contestant does not know what is behind the door s/he
#'  has selceted.
#'
#' @param
#'
#'  No arguments are used by the function.
#'
#' @return
#'
#'  The function returns a numberic vector of length one, incdicating
#'  the door number the contestant has chosen.
#'
#' @examples
#'  select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'
#'  The host opens a goat door.
#'
#' @description
#'
#'  The host opens a door which was not constestant's original choice and
#'  has a goat behind it.
#'
#' @details
#'
#'  The function open_goat_door() uses two 'if' loops to decide which
#'  one of the goat to open. If the contestant's original pick is a 'car',
#'  the function randomly selects one of the goats doors. If the contestant's
#'  original pick is a 'goat', the function eliminates the contestant's choice
#'  door and the car door, to returns a goat door.
#'
#' @param
#'
#'   The function take two arguments
#'   1. Game vector- character vector of length three containig
#'   position of car and goats.
#'   2. a.pick- contains contestant's original pick (numberic vector
#'   of length one containing number between 1-3).
#'
#' @return
#'
#'  The function returns a numeric vector of length one containing a
#'  number between 1 -3.
#'
#' @examples
#'
#'  open_goat_door(game, a.pick)
#'
#'
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
#'
#'  Contestant decides to switch or stay with his/her original choice
#'  of the door.
#'
#' @description
#'
#'  The contestant has a choice to stay to his/her original choice or
#'  switch doors to one of the two availble doors at this point.
#'
#' @details
#'
#'  The contestant decides to stay with his/her original pick or s/he has
#'  an option to switch.If the contestant decide to switch s/he can pick
#'  between one of the two available doors.The function return either the
#'  original pick or one of the two availble doors depending upon
#'  contestant's choice.
#'
#' @param
#'
#'  The function takes three arguments.
#'  1. option- We are considering that constants picks to stay
#'  with original choice as defualt.
#'  2. opened.door - the goat door which is opened by the host
#'  (numberic vector of length one containing number between 1-3).
#'  3. a.pick- Contestent's original pick of the door
#'  (numberic vector of length one containing number between 1-3).
#'
#' @return
#'
#'  Function returns a numeric vector of length one conating a number
#'  between 1-3.
#'
#' @examples
#'
#'  change_door(stay=T, opened.door, a.pick)
#'
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
#'
#' Detemines if the contestant has won or lost the game.
#'
#' @description
#'
#'  The function decides if the contestant has won or lost the
#'  game depending upon his/her final pick of door.
#'
#' @details
#'
#'  The function determines the result of the game. If the contestant's
#'  final pick is car the s/he has won the game. If the final pick is
#'  goat the s/he has lost the game.
#'
#' @param
#'
#'  The function takes two arguments.
#'  1. final.pick- contestant final pick (a number between 1-3)
#'  2. game- character vector of length three containing position
#'  of car and goats.
#'
#' @return
#'
#'  The function returns a character vector of length one with either
#'  "Win" or "lose"
#'
#' @examples
#'
#'  Determine_winner( final.pick, game )
#'
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
#'
#'  A simulation of the Monty Hall problem game.
#'
#' @description
#'
#'  This function simulates all the steps of the game and returns the result.
#'
#' @details
#'
#'  This is a collection of all the steps of the game. It runs a simulation
#'  of the whole game. The function returns the game result for both the
#'  scenarios where the contestant has decided to stay with his/her original
#'  choice and where he has decided to switch doors.
#'
#' @param
#'
#'  No argumnets are required.
#'
#' @return
#'
#'  The function returns a dataframe containing the strategy
#'  (Stay and Switch) and the result in both the scenarios.
#'
#' @examples
#'
#'  play_game()
#'
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
#'
#'   Runs a simulation of the Monty Hall Problem game to
#'   determine the probability of win or lose.
#'
#' @description
#'
#'  The function is a compliation of all the steps in playing
#'  the game. It allows the user to run the simluation for any number
#'  of times to determine probability of win or lose.
#'
#' @details
#'
#'  This function can run the simulation any number of times. It stores
#'  the result in a data frame. After running the simulation for supplied
#'  number of times it calculates the probabilities of win and lose for
#'  both scenarios (stay and switch).
#'
#' @param
#'
#'  The Function takes a number for which the simulation needs to be
#'  run as an argument.
#'
#' @return
#'
#'  The function returns a data frame with probabilities of win and
#'  lose for both scenarios (stay and switch).
#'
#' @examples
#'
#'  play_n_games ( n=100 )
#'
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
