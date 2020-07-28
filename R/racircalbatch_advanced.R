#' Corrects a batch of rapid A/Ci response (RACiR) data
#'
#' \code{racircalbatch_advanced} Corrects your RACiR data files based on a
#' calibration file. Produces diagnostic graphs of A vs. Ci for quality
#' control. Output includes a list of data frames with corrected data.
#'
#' @param data List of data frames with the RACiR response data
#' @param caldata Data frame with the calibration data
#' @param mincut Minimum cutoff value for reference CO2 (CO2_r). Used to cut
#' out the data from the initial chamber mixing. Default value is set to the
#' minimum COR_r value.
#' @param maxcut Maximum cutoff value for reference CO2 (CO2_r). Used to cut
#' out the data from the end of the response. Not needed in all cases. Default
#' value is set to the maximum COR_r value.
#' @param title Vector for titles of output graph - useful for batch RACiR
#' corrections. Length must be equal to data list length
#' @param varnames Variable names - this allows for the use of this code with
#' other machines and setups where variable names may differ.
#' @param digits Specifies rounding for groups. Defaults to -2 (100s). Effectively
#' uses 100 ppm intervals (e.g. data matching >50 ppm to 150 ppm would be assigned
#' to an interval centered around 100 ppm for reference CO2).
#'
#' @return racircalbatch_advanced uses racircal_advanced on many files
#' @importFrom stats BIC
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#' @examples \donttest{
#'#Create a list of files
#'files <- c(system.file("extdata", "poplar_1", package = "racir"),
#'           system.file("extdata", "poplar_2", package = "racir"))
#'data <- vector("list", length(files))
#'for(i in seq_along(files)){
#'  data[[i]] <- read_6800(files[i])
#'  names(data)[i] <- files[i]
#'}
#'
#'caldata <- read_6800(system.file("extdata", "cal", package = "racir"))
#'output <- racircalbatch_advanced(caldata = caldata, data = data,
#'                                 mincut = 300, maxcut = 780, title = files)
#'}
racircalbatch_advanced <- function(caldata, data, mincut, maxcut,
                                   digits, title,
                                   varnames = list(A = "A",
                                                   Ca = "Ca",
                                                   CO2_r = "CO2_r",
                                                   E = "E",
                                                   gtc = "gtc")){
  digits <- ifelse(missing(digits) == TRUE, -2, digits)
  # Check for title
  title <- ifelse(missing(title) == TRUE, rep(NA, length(data)), title)
  output <- vector("list", length(data))
  for (i in seq_along(data)){
    output[[i]] <- racircal_advanced(caldata = caldata,
                   data = data[[i]],
                   mincut = mincut,
                   maxcut = maxcut,
                   title = title[i],
                   digits = digits,
                   varnames = varnames)
    output[[i]]$ID <- title[i]
  }
  return(output)
  }
