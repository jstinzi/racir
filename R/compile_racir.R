#' Compiles .csv file outputs from {racircalbatch}. Will also compile .csv data generally, as long as the headers are the same.
#'
#'\code{compile_racir} Compiles .csv files into a file named "Compiled_Data.csv"
#'
#' @param outputfile Specifies the name for the output file, e.g. "output.csv"
#'
#' @return Compiles .csv files for further analysis
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#' @importFrom dplyr bind_rows
#' @export
#'
compile_racir <- function(outputfile){
  # List .csv files in folder -------------------------------
  files_list <- list.files(pattern = "*.csv")

  # Read .csv files in folder -------------------------------
  racirframes <- lapply(files_list, read.csv)

  # Compile .csv files in folder ----------------------------
  racir_compiled <- do.call(what = bind_rows, args = racirframes)

  # Write compiled data -------------------------------------
  write.csv(racir_compiled, paste(outputfile))
}
