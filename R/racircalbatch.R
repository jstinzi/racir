#' Corrects a batch of rapid A/Ci response (RACiR) data
#'
#' \code{racircal} Corrects your RACiR data files based on a calibration file. Produces diagnostic graphs of A vs. Ci for quality control. Output includes a data frame in the global environment "corrected_data" and a csv file of the form "datafile.csv".
#'
#' @inheritParams racircalcheck
#' @param datafiles Vector of data files to be corrected
#' @param skiplines A number specifying the number of header lines to skip.
#'
#' @return racircalbatch prints .csv files with corrected RACiR data
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
#'
racircalbatch <- function(calfile, mincut, maxcut, datafiles, skiplines){
  for (i in 1:length(datafiles)){
    racircal(calfile, mincut, maxcut, datafiles[i], skiplines)
  }}
