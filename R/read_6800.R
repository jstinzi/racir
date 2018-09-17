#' Reads files from the Li-Cor 6800
#'
#' \code{read_6800} Reads Li-Cor 6800 files, which are delimited by spaces and tabs.
#'
#' @param filename A character string of the form: "mydata".
#'
#' @return read_6800 imports a Li-Cor 6800 file as a data frame
#' @importFrom utils read.delim
#' @export
#'
#'
#'
read_6800 <- function(filename){
  # Read in file for column names ---------------------------
  colname <- read.delim(filename, sep = "\t", skip = 53,
                        header = TRUE, fill = TRUE)

  # Read in file --------------------------------------------
  data <- read.delim(filename, sep = "\t", skip = 55,
                     header = FALSE, fill = TRUE)

  # Assign column names -------------------------------------
  colnames(data) <- colnames(colname)

  # Print data ----------------------------------------------
  return(data)
}
