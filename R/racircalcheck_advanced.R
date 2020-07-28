#' Allows visual checking of rapid A/Ci response (RACiR) calibration data using
#' empty chamber data.
#'
#' \code{racircalcheck_advanced} Used to check range of calibration file.
#' Produces diagnostic graphs of A vs. Ci for quality control.
#'
#' @inheritParams racircalcheck
#' @param digits Specifies rounding for groups. Defaults to -2 (100s). Effectively
#' uses 100 ppm intervals (e.g. data matching >50 ppm to 150 ppm would be assigned
#' to an interval centered around 100 ppm for reference CO2).
#'
#' @return racircalcheck_advanced returns a data frame with corrected RACiR data
#' @importFrom stats BIC
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#' @examples \donttest{
#' #Read in data
#' data <- read_6800(system.file("extdata", "poplar_2", package = "racir"))
#' caldata <- read_6800(system.file("extdata", "cal", package = "racir"))
#' #Correct data
#' racircalcheck_advanced(data = data, mincut = 350, maxcut = 780)
#' }
#'
#'
racircalcheck_advanced <- function(data, mincut, maxcut,
                                   digits,
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
  digits <- ifelse(missing(digits) == TRUE, -2, digits)
  #Create intervals
  data$sep_columns <- round(data$CO2_r, digits = digits)

  # Assign cutoffs ------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(data$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(data$CO2_r), maxcut)
  data <- data[data$CO2_r > mincut, ]
  data <- data[data$CO2_r < maxcut, ]

  range.split <- split(data, data$sep_columns)
  for(i in seq_along(range.split)){
    if(nrow(range.split[[i]]) < 6){
      error_message <- paste("Interval",
                             names(range.split)[i],
                             "ppm has insufficient observations (<6) for the
advanced correction to work. Please adjust mincut or maxcut.")
      stop(error_message)
    }
  }
  for (i in seq_along(range.split)){
  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = range.split[[i]])
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = range.split[[i]])
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = range.split[[i]])
  cal4th <- lm(A ~ poly(CO2_r, 4), data = range.split[[i]])
  cal5th <- lm(A ~ poly(CO2_r, 5), data = range.split[[i]])
  range.split[[i]]$predict1 <- predict(cal1st)
  range.split[[i]]$predict2 <- predict(cal2nd)
  range.split[[i]]$predict3 <- predict(cal3rd)
  range.split[[i]]$predict4 <- predict(cal4th)
  range.split[[i]]$predict5 <- predict(cal5th)
  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])
  print(best)
  }
  data <- do.call("rbind", range.split)
  # Plot calibration data and curve fits --------------------
  plot(data$A ~ data$CO2_r, main = "Calibration Fits")
  lines(data$CO2_r, data$predict1, col = "green")
  lines(data$CO2_r, data$predict2, col = "blue")
  lines(data$CO2_r, data$predict3, col = "red")
  lines(data$CO2_r, data$predict4, col = "grey")
  lines(data$CO2_r, data$predict5, col = "black")
  legend("bottom", horiz = TRUE, title = "Polynomial",
         legend = c("1st", "2nd", "3rd", "4th", "5th"),
         col = c("green", "blue", "red", "grey", "black"),
         lty = 1, cex = 0.6)
  }
