#' Reads files from the Walz system
#'
#' \code{read_Walz} Reads Walz files, which are delimited by commas.
#'
#' @param filename A character string of the form: "mydata".
#' @param skiplines A number specifying the number of header lines to skip.
#'
#' @return read_Walz imports a Walz file as a data frame
#' @importFrom utils read.csv
#' @export
#'
#'
#'
read_Walz <- function(filename, skiplines){
  skiplines <- ifelse(missing(skiplines) == TRUE, 0, skiplines)
  # Read in file for column names ---------------------------
  colname <- read.csv(filename, skip = skiplines,
                        header = TRUE, fill = TRUE)

  # Read in file --------------------------------------------
  data <- read.csv(filename, skip = skiplines+2,
                     header = FALSE, fill = TRUE)

  # Assign column names -------------------------------------
  colnames(data) <- colnames(colname)

  # Print data ----------------------------------------------
  return(data)
}
