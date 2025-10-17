#' Barplots of numbers picked from given samples
#'
#' @param n a positive number, the sample size
#' @param iter a positive number, the number of samples
#' @param time a positive integer, the time between each sample in seconds
#' @importFrom grDevices rainbow
#'
#' @returns barplots showing the distribution of what numbers were picked from n trials from numbers 1-10 from iter number of samples. The barplots are separated by time seconds.
#' @export
#'
#' @examples
#' mysample(n = 1000, iter = 30)
#' mysample(n = 1000, iter = 1, time = 1)

mysample = function(n, iter = 10, time = 0.5){
  for( i in 1:iter){

    #make a sample
    s = sample(1:10, n, replace = TRUE)

    # turn the sample into a factor
    sf = factor(s, levels = 1:10)

    #make a barplot
    barplot(table(sf)/n, beside = TRUE, col = rainbow(10),
            main = paste("Example sample()", " iteration ", i, " n = ", n, sep = ""),
            ylim = c(0, 0.2))

    #release the table
    Sys.sleep(time)
  }
}
