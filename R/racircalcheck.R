#' Allows visual checking of rapid A/Ci response (RACiR) calibration data using
#' empty chamber data.
#'
#' \code{racircalcheck} Used to check range of calibration file. Produces
#' diagnostic graphs of A vs. Ci for quality control. Output includes plots
#' for checking and confirming cutoff values, and a plot with the fit, as well
#' as information as to which polynomial fit the data best.
#'
#' @param data Data frame with the calibration (empty chamber) rapid A/Ci response
#' @param mincut Minimum cutoff value for reference CO2 (CO2_r). Used to cut
#' out the data from the initial chamber mixing. Default value is set to the
#' minimum COR_r value.
#' @param maxcut Maximum cutoff value for reference CO2 (CO2_r). Used to cut
#' out the data from the end of the response. Not needed in all cases. Default
#' value is set to the maximum COR_r value.
#' @param varnames Variable names - this allows for the use of this code with
#' other machines and setups where variable names may differ.
#'
#' @return racircalcheck allows visual checking of RACiR calibration data
#' @importFrom utils read.delim
#' @importFrom stats BIC
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#' @examples \donttest{
#' #Read in the file
#' data <- read_6800(system.file("extdata", "cal", package = "racir"))
#' #Run calibration check
#' racircalcheck(data = data,
#'               mincut = 350,
#'               maxcut = 780)
#' }
#'
racircalcheck <- function(data, mincut, maxcut,
                          varnames = list(A = "A",
                                          Ca = "Ca",
                                          CO2_r = "CO2_r",
                                          E = "E",
                                          gtc = "gtc")){
  #assign variable names
  data$A <- data[, varnames$A]
  data$Ca <- data[, varnames$Ca]
  data$CO2_r <- data[, varnames$CO2_r]
  data$E <- data[, varnames$E]
  data$gtc <- data[, varnames$gtc]
  # Check data for cutoffs ----------------------------------
  plot(A~CO2_r, data = data, main = "Check cutoffs")
  # Assign cutoffs ------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(data$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(data$CO2_r), maxcut)
  data <- data[data$CO2_r > mincut, ]
  data <- data[data$CO2_r < maxcut, ]
  # Confirm cutoffs are appropriate -------------------------
  plot(A~CO2_r, data = data, main = "Confirm cutoffs")
  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = data)
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = data)
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = data)
  cal4th <- lm(A ~ poly(CO2_r, 4), data = data)
  cal5th <- lm(A ~ poly(CO2_r, 5), data = data)
  predict1 <- predict(cal1st)
  predict2 <- predict(cal2nd)
  predict3 <- predict(cal3rd)
  predict4 <- predict(cal4th)
  predict5 <- predict(cal5th)
  # Plot calibration data and curve fits --------------------
  plot(data$A ~ data$CO2_r, main = "Calibration Fits")
  lines(data$CO2_r, predict1, col = "green")
  lines(data$CO2_r, predict2, col = "blue")
  lines(data$CO2_r, predict3, col = "red")
  lines(data$CO2_r, predict4, col = "grey")
  lines(data$CO2_r, predict5, col = "black")
  legend("bottom", horiz = TRUE, title = "Polynomial",
         legend = c("1st", "2nd", "3rd", "4th", "5th"),
         col = c("green", "blue", "red", "grey", "black"),
         lty = 1, cex = 0.6)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])
  print(best)
}
