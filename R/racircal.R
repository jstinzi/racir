#' Corrects rapid A/Ci response (RACiR) data from leaves using empty chamber data.
#'
#' \code{racircal} Corrects your RACiR data files based on a calibration file. Produces diagnostic graphs of A vs. Ci for quality control. Output includes a data frame in the global environment "corrected_data" and a csv file of the form "datafile.csv".
#'
#' @inheritParams racircalcheck
#' @param datafile Name of the data file to be corrected
#'
#' @return racircal returns a data frame with corrected RACiR data
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
racircal <- function(calfile, mincut, maxcut, datafile, skiplines){
  # Load calibration data -----------------------------------
  cal <- read_6800(calfile, skiplines)

  # Assign cutoffs ------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(cal$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(cal$CO2_r), maxcut)
  cal <- cal[cal$CO2_r > mincut, ]
  cal <- cal[cal$CO2_r < maxcut, ]

  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = cal)
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = cal)
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = cal)
  cal4th <- lm(A ~ poly(CO2_r, 4), data = cal)
  cal5th <- lm(A ~ poly(CO2_r, 5), data = cal)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])

  # Assigns maximum and minimum CO2_r values based on -------
  # calibration data ----------------------------------------
  maxcal <- max(cal$CO2_r)
  mincal <- min(cal$CO2_r)

  # Read leaf data file -------------------------------------
  id <- read_6800(datafile, skiplines)

  # Restrict data to calibration range ----------------------
  id <- id[id$CO2_r > mincal, ]
  id <- id[id$CO2_r < maxcal, ]

  # Correct leaf data ---------------------------------------
  ifelse(best == "cal5th", id$Acor <- id$A - predict(cal5th, id),
         ifelse(best == "cal4th", id$Acor <- id$A - predict(cal4th, id),
         ifelse(best == "cal3rd", id$Acor <- id$A - predict(cal3rd, id),
         ifelse(best == "cal2nd", id$Acor <- id$A - predict(cal2nd, id),
            id$Acor <- id$A - predict(cal1st, id)))))
  id$Cicor <- ( ( (id$gtc - id$E / 2) * id$Ca - id$Acor) /
                  (id$gtc + id$E / 2))

  # Plot corrected leaf data --------------------------------
  plot(Acor ~ Cicor, data = id)

  # Add ID label to file ------------------------------------
  id$ID <- rep(datafile, length(id$obs))

  # Remove columns filled with NA ---------------------------
  id1 <- id[, unlist(lapply(id, function(x) !all(is.na(x))))]

  # Write data output to .csv -------------------------------
  write.csv(id1, paste(datafile, ".csv", sep = ""))
}
