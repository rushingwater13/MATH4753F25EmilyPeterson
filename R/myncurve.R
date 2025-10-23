#' A Normal Distribution plot with shaded probability P(Y <= a)
#'
#' @param mu any real number, the mean of the distribution
#' @param sigma any positive number, the standard deviation of the distribution
#' @param a any real number, the value of the desired probability
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @returns a plot showing the distribution curve with the probability area shaded and a list of the mean, standard deviation, and area values
#' @export
#'
#' @examples
#' myncurve(2, 3, 1)
myncurve = function(mu, sigma, a){

  # make sure the sigma value is valid
  if(sigma <= 0){
    stop("sigma must be a positive number.", call. = FALSE)
  }

  # Draw the curve with the provided values
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3*sigma, mu + 3*sigma))

  # record the probability, also the area under the curve
  prob = pnorm(a, mu, sigma)

  # trace out the area as a polygon
  xcurve = seq(mu - 4*sigma, a, length = 100000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(x = c(mu - 4*sigma, xcurve, a),
          y = c(0, ycurve, 0), col = "Light Green")

  # print the area in the polygon, based on where the cutoff is
  prob2 = round(prob, 4)
  text(a - sigma, 0.3*dnorm(a-0.5*sigma, mu, sigma),
       paste0("Area = ", prob2))

  list(mu = mu, sigma = sigma, area = prob2)
}

