#' Corrects rapid A/Ci response (RACiR) data from leaves using empty chamber
#' data.
#'
#' \code{racircal} Corrects your RACiR data based on calibration data. Produces
#' corrected A vs. Ci graph. Output is a data frame with corrected RACiR data
#' using variable names Acor and Cicor for the corrected A and Ci values.
#' @param data Data frame with the RACiR response data
#' @param caldata Data frame with the calibration data
#' @param mincut Minimum cutoff value for reference CO2 (CO2_r). Used to cut
#' out the data from the initial chamber mixing. Default value is set to the
#' minimum COR_r value.
#' @param maxcut Maximum cutoff value for reference CO2 (CO2_r). Used to cut
#' out the data from the end of the response. Not needed in all cases. Default
#' value is set to the maximum COR_r value.
#' @param title Title of output graph - useful for batch RACiR corrections.
#' @param varnames Variable names - this allows for the use of this code with
#' other machines and setups where variable names may differ.
#'
#' @return racircal returns a data frame with corrected RACiR data
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
#' data_corrected <- racircal(data = data, caldata = caldata,
#'                            mincut = 350, maxcut = 780, title = "Test")
#' }
#'
racircal <- function(data, caldata, mincut, maxcut, title,
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

  # Check for title
  title <- ifelse(missing(title) == TRUE, NA, title)
  # Assign cutoffs ------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(caldata$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(caldata$CO2_r), maxcut)
  caldata <- caldata[caldata$CO2_r > mincut, ]
  caldata <- caldata[caldata$CO2_r < maxcut, ]

  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = caldata)
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = caldata)
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = caldata)
  cal4th <- lm(A ~ poly(CO2_r, 4), data = caldata)
  cal5th <- lm(A ~ poly(CO2_r, 5), data = caldata)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])

  # Assigns maximum and minimum CO2_r values based on -------
  # calibration data ----------------------------------------
  maxcal <- max(caldata$CO2_r)
  mincal <- min(caldata$CO2_r)
  # Restrict data to calibration range ----------------------
  data <- data[data$CO2_r > mincal, ]
  data <- data[data$CO2_r < maxcal, ]

  # Correct leaf data ---------------------------------------
  ifelse(best == "cal5th", data$Acor <- data$A - predict(cal5th, data),
         ifelse(best == "cal4th", data$Acor <- data$A - predict(cal4th, data),
         ifelse(best == "cal3rd", data$Acor <- data$A - predict(cal3rd, data),
         ifelse(best == "cal2nd", data$Acor <- data$A - predict(cal2nd, data),
            data$Acor <- data$A - predict(cal1st, data)))))
  data$Cicor <- ( ( (data$gtc - data$E / 2) * data$Ca - data$Acor) /
                  (data$gtc + data$E / 2))
  plot(Acor ~ Cicor, data = data, main = title)
  # Remove columns filled with NA ---------------------------
  output <- data[, unlist(lapply(data, function(x) !all(is.na(x))))]
  # Return data frame
  return(output)
}
