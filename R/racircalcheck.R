#' Allows visual checking of rapid A/Ci response (RACiR) calibration data using empty chamber data.
#'
#' \code{racircalcheck} Used to check range of calibration file. Produces diagnostic graphs of A vs. Ci for quality control. Output includes a data frame in the global environment "corrected_data" and a csv file of the form "datafile.csv".
#'
#' @param calfile Calibration (empty chamber) rapid A/Ci response file
#' @param mincut Minimum cutoff value for reference CO2 (CO2_r). Used to cut out the data from the initial chamber mixing. Default value is set to the minimum COR_r value.
#' @param maxcut Maximum cutoff value for reference CO2 (CO2_r). Used to cut out the data from the end of the response. Not needed in all cases. Default value is set to the maximum COR_r value.
#'
#' @return racircal returns a data frame with corrected RACiR data
#' @importFrom utils read.delim
#' @importFrom stats BIC
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#'
#'
#'
racircalcheck <- function(calfile, mincut, maxcut, skiplines){
  # Load calibration data -----------------------------------
  cal <- read_6800(calfile, skiplines)

  # Check data for cutoffs ----------------------------------
  plot(A~CO2_r, data = cal, main = "Check cutoffs")

  # Assign cutoffs ------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(cal$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(cal$CO2_r), maxcut)
  cal <- cal[cal$CO2_r > mincut, ]
  cal <- cal[cal$CO2_r < maxcut, ]

  # Confirm cutoffs are appropriate -------------------------
  plot(A~CO2_r, data = cal, main = "Confirm cutoffs")

  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = cal)
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = cal)
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = cal)
  cal4th <- lm(A ~ poly(CO2_r, 4), data = cal)
  cal5th <- lm(A ~ poly(CO2_r, 5), data = cal)
  predict1 <- predict(cal1st)
  predict2 <- predict(cal2nd)
  predict3 <- predict(cal3rd)
  predict4 <- predict(cal4th)
  predict5 <- predict(cal5th)

  # Plot calibration data and curve fits --------------------
  plot(cal$A ~ cal$CO2_r, main = "Calibration Fits")
  lines(cal$CO2_r, predict1, col = "green")
  lines(cal$CO2_r, predict2, col = "blue")
  lines(cal$CO2_r, predict3, col = "red")
  lines(cal$CO2_r, predict4, col = "grey")
  lines(cal$CO2_r, predict5, col = "black")
  legend("bottom", horiz = TRUE, title = "Polynomial",
         legend = c("1st", "2nd", "3rd", "4th", "5th"),
         col = c("green", "blue", "red", "grey", "black"), lty = 1)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])
  print(best)
}
