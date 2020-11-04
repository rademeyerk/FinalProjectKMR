#' 10-Day Weather Simulation (Richmond, VA)
#'
#' This function uses a simple Markov chain process to simulate weather based on 
#' a probability matrix that was pre-set. The output of the function is a dataframe
#' of the 10 day forecast. Included is total rainfall, as well as the total number of
#' sunny days for the 10 day period.
#' 
#' @param Initial is the starting state. It can be "Sunny" or "Rainy." 
#' This determines the start state vector that is used in the function.
#' @export
#' 
#' @examples RVATenDayWeather("Sunny")

RVATenDayWeather <- function(Initial) {
  ProbMatrix <- matrix(c(0.85, 0.15, 0.65, 0.35), 2, 2, T)
  StartState <- Initial
  ifelse(StartState == "Sunny", InitialVector <- c(1, 0), InitialVector <- c(0, 1))
  ForecastDF <- data.frame("Day" = 1:10, "Prediction" = rep(0, 10), "Rainfall" = rep(0, 10))
  for (i in 1:10) {
    nextDay <- InitialVector %*% ProbMatrix
    NextDaySim <- rbernoulli(1, p = nextDay[1])
    ifelse(NextDaySim == 1, ForecastDF$Prediction[i] <- "Sun", ForecastDF$Prediction[i] <- "Rain")
    ifelse(NextDaySim == 0, ForecastDF$Rainfall[i] <- rexp(1, 2), ForecastDF$Rainfall[i] <- round(0, 1))
    nextDay <- nextDay %*% ProbMatrix
  }
  
  ForecastDF %>% mutate("Sunny Days" = cumsum(Prediction == "Sun"), "Total Rainfall" = cumsum(Rainfall))
}

