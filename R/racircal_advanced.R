#' Corrects rapid A/Ci response (RACiR) data from leaves using empty chamber data.
#'
#' \code{racircal_advanced} Interval correction for RACiR data.
#'
#' @inheritParams racircal
#' @param digits Specifies rounding for groups. Defaults to -2 (100s). Effectively
#' uses 100 ppm intervals (e.g. data matching >50 ppm to 150 ppm would be assigned
#' to an interval centered around 100 ppm for reference CO2).
#'
#' @return racircal_advanced racircalcheck allows visual checking of RACiR
#' calibration data
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
#' data_corrected <- racircal_advanced(data = data, caldata = caldata,
#'                                     mincut = 350, maxcut = 780,
#'                                     digits = -2, title = "Test")
#' }
#'
racircal_advanced <- function(data, caldata, mincut, maxcut, title,
                              digits, varnames = list(A = "A",
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
  # Check for title
  title <- ifelse(missing(title) == TRUE, NA, title)
  #Add separation column --------------------------------------
  caldata$sep_columns <- round(caldata$CO2_r, digits = digits)
  data$sep_columns <- round(data$CO2_r, digits = digits)
  # Assign cutoffs --------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(caldata$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(caldata$CO2_r), maxcut)
  caldata <- caldata[caldata$CO2_r > mincut, ]
  caldata <- caldata[caldata$CO2_r < maxcut, ]
  # Assigns maximum and minimum CO2_r values based on ---------
  # calibration data ------------------------------------------
  maxcal <- max(caldata$CO2_r)
  mincal <- min(caldata$CO2_r)
  data <- data[data$CO2_r > mincal, ]
  data <- data[data$CO2_r < maxcal, ]
  #Split data based on CO2 ranges -----------------------------
  range.split_cal <- split(caldata, caldata$sep_columns)
  range.split_data <- split(data, data$sep_columns)

  #Check for overlap between data and calibration file
  intervals <- names(range.split_data) %in% names(range.split_cal)
  int_position <- 1:length(intervals)
  intervals <- data.frame(intervals, int_position)
  for(i in 1:nrow(intervals)){
    if(intervals$intervals[i] == FALSE){
      intervals$intervals[i] <- NA
    }
  }
  intervals <- intervals[is.na(intervals$intervals) == FALSE,]
  range.split_cal <- range.split_cal[intervals$int_position]
  range.split_data <- range.split_data[intervals$int_position]

  for(i in seq_along(range.split_data)){
      if(nrow(range.split_cal[[i]]) < 6 |
         nrow(range.split_data[[i]]) < 6 ){
        error_message <- paste("Interval",
        names(range.split_data)[i],
"ppm has insufficient observations (<6) for the
advanced correction to work. Please adjust mincut or maxcut.")
        stop(error_message)
      }
  }

  #Model calibration and correct leaf data --------------------
  for(i in seq_along(range.split_data)){
    cal1st <- lm(A ~ CO2_r, data = range.split_cal[[i]])
    cal2nd <- lm(A ~ poly(CO2_r, 2), data = range.split_cal[[i]])
    cal3rd <- lm(A ~ poly(CO2_r, 3), data = range.split_cal[[i]])
    cal4th <- lm(A ~ poly(CO2_r, 4), data = range.split_cal[[i]])
    cal5th <- lm(A ~ poly(CO2_r, 5), data = range.split_cal[[i]])
    # Use BIC to assess best polynomial -----------------------
    bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
    best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])
    # Correct leaf data ---------------------------------------
    ifelse(best == "cal5th", range.split_data[[i]]$Acor <-
            range.split_data[[i]]$A - predict(cal5th, range.split_data[[i]]),
      ifelse(best == "cal4th", range.split_data[[i]]$Acor <-
            range.split_data[[i]]$A - predict(cal4th, range.split_data[[i]]),
      ifelse(best == "cal3rd", range.split_data[[i]]$Acor <-
            range.split_data[[i]]$A - predict(cal3rd, range.split_data[[i]]),
      ifelse(best == "cal2nd", range.split_data[[i]]$Acor <-
            range.split_data[[i]]$A - predict(cal2nd, range.split_data[[i]]),
        range.split_data[[i]]$Acor <-
          range.split_data[[i]]$A - predict(cal1st, range.split_data[[i]])))))
    range.split_data[[i]]$Cicor = ( ( (range.split_data[[i]]$gtc -
                     range.split_data[[i]]$E / 2) *
                     range.split_data[[i]]$Ca - range.split_data[[i]]$Acor) /
                    (range.split_data[[i]]$gtc + range.split_data[[i]]$E / 2))
  }
  #Recombine data -------------------------------------------
  data <- do.call("rbind", range.split_data)
  # Plot corrected leaf data --------------------------------
  plot(Acor ~ Cicor, data = data, main = title)
  # Plot A vs Cs as well
  plot(Acor ~ CO2_s, data = data, main = title)
  # Remove columns filled with NA ---------------------------
  output <- data[, unlist(lapply(data, function(x) !all(is.na(x))))]
  # Return output
  return(output)
}
