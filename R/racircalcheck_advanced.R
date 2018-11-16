#' Allows visual checking of rapid A/Ci response (RACiR) calibration data using empty chamber data.
#'
#' \code{racircalcheck_advanced} Used to check range of calibration file. Produces diagnostic graphs of A vs. Ci for quality control. Output includes a data frame in the global environment "corrected_data" and a csv file of the form "datafile.csv".
#'
#' @param calfile Calibration (empty chamber) rapid A/Ci response file
#' @param mincut Minimum cutoff value for reference CO2 (CO2_r). Used to cut out the data from the initial chamber mixing. Default value is set to the minimum COR_r value.
#' @param maxcut Maximum cutoff value for reference CO2 (CO2_r). Used to cut out the data from the end of the response. Not needed in all cases. Default value is set to the maximum COR_r value.
#' @param skiplines A number specifying the number of header lines to skip.
#' @param filetype Specifies input datatype. Defaults to 6800.
#' @param digits Specifies rounding for groups. Defaults to -2 (100s).
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
racircalcheck_advanced <- function(calfile, mincut, maxcut, skiplines, filetype, digits){
  #Defaults ---------------------------------------------------
  filetype <- ifelse(missing(filetype) == TRUE, 6800, filetype)
  skiplines <- ifelse(missing(skiplines) == TRUE, 53, skiplines)
  digits <- ifelse(missing(digits) == TRUE, -2, digits)
  # Load data -------------------------------------------------
  ifelse(filetype == 6800, cal <- read_6800(calfile, skiplines),
         ifelse(filetype == 'csv', cal <- read.csv(calfile),
                ifelse(filetype == 'dataframe', cal <- calfile,
                       "Error: filetype not recognized")))
  cal$sep_columns <- round(cal$CO2_r, digits = digits)

  # Assign cutoffs ------------------------------------------
  mincut <- ifelse(missing(mincut) == TRUE, min(cal$CO2_r), mincut)
  maxcut <- ifelse(missing(maxcut) == TRUE, max(cal$CO2_r), maxcut)
  cal <- cal[cal$CO2_r > mincut, ]
  cal <- cal[cal$CO2_r < maxcut, ]

  range.split <- split(cal, cal$sep_columns)
  for (i in 1:length(range.split)){
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
  cal <- do.call("rbind", range.split)
  # Plot calibration data and curve fits --------------------
  plot(cal$A ~ cal$CO2_r, main = "Calibration Fits")
  lines(cal$CO2_r, cal$predict1, col = "green")
  lines(cal$CO2_r, cal$predict2, col = "blue")
  lines(cal$CO2_r, cal$predict3, col = "red")
  lines(cal$CO2_r, cal$predict4, col = "grey")
  lines(cal$CO2_r, cal$predict5, col = "black")
  legend("bottom", horiz = TRUE, title = "Polynomial",
         legend = c("1st", "2nd", "3rd", "4th", "5th"),
         col = c("green", "blue", "red", "grey", "black"), lty = 1)
  }
