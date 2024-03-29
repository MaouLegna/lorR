% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lor_leaderboard.R
\name{lor_leaderboard}
\alias{lor_leaderboard}
\title{Return Leaderboard}
\usage{
lor_leaderboard(server, names = FALSE, ...)
}
\arguments{
\item{server, }{a character, must be one of americas,europe,sea or asia,apac}

\item{names}{a logical, is TRUE return only the a vector of players names.}

\item{...}{additional paramter for RETRY function, at the moment are timeout, times, pause_base, pause_cap, pause_min,}
}
\value{
a data frame n x3 with n equal to the number of current Master players
name - gameName of the player
rank - rank on the leaderboard starting from 0 to n-1
lp - League Points of a player
}
\description{
Return the leaderboard of Master players
Wraps the \href{https://developer.riotgames.com/apis#lor-ranked-v1/GET_getLeaderboards}{leaderboard} api method.
The leaderboard is updated once an hour.
}
\details{
Standard RATE LIMITS
\emph{20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key}
\emph{500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key}

Method RATE LIMITS
\emph{X-Method-Rate-Limit": "30:10,500:600 - Developer Key}
\emph{X-Method-Rate-Limit": "500:10,30000:600 - Production Key}
}
\examples{
\dontrun{
# lor_leaderboard("europe",names = TRUE)
# lor_leaderboard("americas")
}
}
