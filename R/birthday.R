#' The probability of shared birthdays
#'
#' @param x a quantitative variable
#'
#' @returns the probability of two or more people in a group of x having the same birthday
#' @export
#'
#' @examples
#' birthday(20:24)
birthday <- function(x) {
  1 - exp(lchoose(365, x) + lfactorial(x) - x * log(365))
}
