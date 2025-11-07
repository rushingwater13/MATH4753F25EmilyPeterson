#' Calculate Tickets To Be Sold For a Flight
#'
#' @param N the number of seats in a flight
#' @param gamma the probability a flight will be truly overbooked
#' @param p the probability of someone who booked a ticket actually showing up
#' @importFrom graphics abline lines points
#' @importFrom stats optimize pbinom
#'
#' @returns two plots showing the discrete and continuous cases of calculating how many tickets to sell and a list of the values
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p) {

  # For the Discrete case

  # create the range of numbers to check
  seqToCheck <- seq(N, N + 20, by = 1)

  # make the sequence of probabilities to compare
  probsOfSeq <- (1 - gamma) - pbinom(N, seqToCheck, p)

  # find the root of the function of probabilities
  absProbs <- abs(probsOfSeq)
  indexOfMin <- which.min(absProbs)
  minValue <- seqToCheck[indexOfMin]



  # For the Continuous case

  # a function to calculate the probabilities
  findMin <- function(n) {
    probsOfNorm <- (1 - gamma) - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
  }

  # a function to find the absolute value of the probabilities curve
  absNorm <- function(n) {
    abs(findMin(n))
  }

  # find the root of the curve
  minNorm <- optimize(absNorm, interval = c(N, N + 20))$minimum



  #Plots

  # create a plot for the discrete case
  plot(seqToCheck, probsOfSeq,
       xlab = "n", ylab = "Objective",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",
                     minValue, ") gamma=", gamma, " N=", N, " discrete"))

  # the actual curve
  lines(seqToCheck, probsOfSeq, col = "black", lwd = 1)
  points(seqToCheck, probsOfSeq, col = "blue", pch = 19)

  # the cross hair
  abline(v = minValue, col = "red", lwd = 3)
  abline(h = 0, col = "red", lwd = 3)
  points(minValue, probsOfSeq[indexOfMin], col = "red", pch = 19, cex = 1.5)



  # create a plot for the continuous case
  curve(findMin(x), N, N + 20,
        xlab = "n", ylab = "Objective",
        main = paste0("Objective Vs n to find optimal tickets sold\n(",
                      minNorm, ") gamma=", gamma, " N=", N, " continuous"))

  # the cross hair
  abline(v = minNorm, col = "blue", lwd = 1)
  abline(h = 0, col = "blue", lwd = 1)




  # the list to print out
  list(nd = minValue, nc = minNorm, N = N, p = p, gamma = gamma)
}
