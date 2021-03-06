% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{The host opens a goat door.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{The}{function take two arguments
1. Game vector- character vector of length three containig
position of car and goats.
2. a.pick- contains contestant's original pick (numberic vector
of length one containing number between 1-3).}
}
\value{
The function returns a numeric vector of length one containing a
 number between 1 -3.
}
\description{
The host opens a door which was not constestant's original choice and
 has a goat behind it.
}
\details{
The function open_goat_door() uses two 'if' loops to decide which
 one of the goat to open. If the contestant's original pick is a 'car',
 the function randomly selects one of the goats doors. If the contestant's
 original pick is a 'goat', the function eliminates the contestant's choice
 door and the car door, to returns a goat door.
}
\examples{

 open_goat_door(game, a.pick)


}
