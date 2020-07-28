#' Reads files from the Li-Cor 6800
#'
#' \code{read_6800} Reads Li-Cor 6800 files, which are delimited by spaces and tabs.
#'
#' @param x A Li-Cor 6800 data file name of the form: "mydata".
#'
#' @return read_6800 imports a Li-Cor 6800 file as a data frame
#' @importFrom utils read.delim
#' @export
#'
#'
#'

#read_6800 <- function(x) {
#  #Read in header information
#  header <- read.delim(file = x, header = TRUE, sep = "\t",
#                     skip = grep(pattern = "\\[Data\\]",
#                                 x = readLines(x),
#                                 value = FALSE) + 1,
#                     nrows = 1)
#  #Read in data information
#  data <- read.delim(file = x, header = FALSE, sep = "\t",
#                   skip = grep(pattern = "\\[Data\\]",
#                               x = readLines(x),
#                               value = FALSE) + 3)
#  #Add header to data
#  colnames(data) <- colnames(header)
#  #Return data
#  return(data)
#}


read_6800 <- function(x) {
  #Read in header information
  header <- read.delim(file = x, header = TRUE, sep = "\t",
                       skip = grep(pattern = "\\[Data\\]",
                                   perl = TRUE,
                                   useBytes = TRUE,
                                   x = readLines(x),
                                   value = FALSE),
                       nrows = 1)
  #Read in data information
  data <- read.delim(file = x, header = FALSE, sep = "\t",
                     skip = grep(pattern = "\\[Data\\]",
                                 perl = TRUE,
                                 useBytes = TRUE,
                                 x = readLines(x),
                                 value = FALSE) + 3)
  #Add header to data
  colnames(data) <- header[1,]
  #Return data
  return(data)
}
