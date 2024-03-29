% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_puuid_from_riotId.R
\name{get_puuid_from_riotId}
\alias{get_puuid_from_riotId}
\title{Recover the PUUID from a riotID}
\usage{
get_puuid_from_riotId(
  gameName,
  server = "americas",
  tagLine,
  format = "parsed",
  ...
)
}
\arguments{
\item{gameName}{a character, string for gameName}

\item{server}{a character, must be one of americas,europe,sea or asia,apac}

\item{tagLine}{a character, string for tagLine}

\item{format}{a character, format of the output, must be:
parsed - as a vector of only the puuid
text   - as the original json from the API request}

\item{...}{additional paramter for RETRY function, at the moment are timeout, times, pause_base, pause_cap, pause_min,}
}
\value{
depending on the format chosen return the information for the PUUID When encountering a status code different from 200 the output is NA
}
\description{
Given the secondary key, a RiotID, return the PUUID of a player.
Wraps the \href{"https://developer.riotgames.com/apis#account-v1/GET_getByRiotId"}{GET_getByRiotId} api method.
}
\details{
Standard RATE LIMITS
\emph{20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key}
\emph{500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key}

Method RATE LIMITS
\emph{1000 requests every 1 minutes - Developer Key}
\emph{1000 requests every 1 minutes - Production Key}
}
\examples{
\dontrun{
lorR::get_puuid_from_riotId(gameName = "MaouLegna",tagLine = "STAT")
}
}
