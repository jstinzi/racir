#' Corrects rapid A/Ci response (RACiR) data from leaves using empty chamber data.
#'
#' \code{racircal_advanced} Experimental correction for RACiR data.
#'
#' @inheritParams racircalcheck
#' @param datafile Name of the data file to be corrected.
#' @param skiplines A number specifying the number of header lines to skip.
#' @param filetype Specifies input datatype. Defaults to 6800.
#' @param digits Specifies rounding for groups. Defaults to -2 (100s).
#'
#' @return racircal_advanced returns a data frame with corrected RACiR data
#' @importFrom utils write.csv
#' @importFrom utils read.delim
#' @importFrom stats BIC
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#'
racircal_advanced <- function(calfile, mincut, maxcut, datafile, skiplines,
                              filetype, digits){
  #Defaults ---------------------------------------------------
  filetype <- ifelse(missing(filetype) == TRUE, 6800, filetype)
  skiplines <- ifelse(missing(skiplines) == TRUE, 53, skiplines)
  digits <- ifelse(missing(digits) == TRUE, -2, digits)
  # Load data -------------------------------------------------
  ifelse(filetype == 6800, cal <- read_6800(calfile, skiplines),
         ifelse(filetype == 'csv', cal <- read.csv(calfile),
                ifelse(filetype == 'dataframe', cal <- calfile,
                       "Error: filetype not recognized")))
  ifelse(filetype == 6800, dataframe <- read_6800(datafile, skiplines),
         ifelse(filetype == 'csv', dataframe <- read.csv(datafile),
                ifelse(filetype == 'dataframe', dataframe <- datafile,
                       "Error: filetype not recognized")))
  #Add separation column --------------------------------------
  cal$sep_columns <- round(cal$CO2_r, digits = digits)
  dataframe$sep_columns <- round(dataframe$CO2_r, digits = digits)
  # Assign cutoffs --------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(cal$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(cal$CO2_r), maxcut)
  cal <- cal[cal$CO2_r > mincut, ]
  cal <- cal[cal$CO2_r < maxcut, ]
  # Assigns maximum and minimum CO2_r values based on ---------
  # calibration data ------------------------------------------
  maxcal <- max(cal$CO2_r)
  mincal <- min(cal$CO2_r)
  dataframe <- dataframe[dataframe$CO2_r > mincal, ]
  dataframe <- dataframe[dataframe$CO2_r < maxcal, ]
  #Split data based on CO2 ranges -----------------------------
  range.split_cal <- split(cal, cal$sep_columns)
  range.split_data <- split(dataframe, dataframe$sep_columns)
  #Model calibration and correct leaf data --------------------
  for(i in 1:length(range.split_data)){
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
  id <- do.call("rbind", range.split_data)
  # Plot corrected leaf data --------------------------------
  plot(Acor ~ Cicor, data = id, main = datafile)
  # Plot A vs Cs as well
  plot(Acor ~ CO2_s, data = is, main = datafile)
  # Add ID label to file ------------------------------------
  id$ID <- rep(datafile, length(id$obs))
  # Remove columns filled with NA ---------------------------
  id1 <- id[, unlist(lapply(id, function(x) !all(is.na(x))))]
  # Write data output to .csv -------------------------------
  write.csv(id1, paste(datafile, ".csv", sep = ""))
}
