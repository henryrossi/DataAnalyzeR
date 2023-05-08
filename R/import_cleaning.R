#' Loads a file given a format, with optional treatment of NA values
#'
#' @param file The name of the file to be imported
#' @param file The type of the file ("csv" = (.csv), "excel" = (.xls, .xlsx) "text" = (txt))
#' @param removeNA a boolean to remove rows with NA values
#' @param removeDuplicates a boolean to remove duplicate rows
#' @param cleanNames a boolean to clean the column names using the janitor package
#' @param delimeter a character representing the delimeter if a text file is being read in
#'
#' @importFrom readxl read_excel
#' @importFrom utils read.table read.csv
#'
#' @return A cleaned dataframe
#'
#' @export
cleanImport <- function(file, type,
                        removeNA = TRUE,
                        removeDuplicates = TRUE,
                        cleanNames = TRUE,
                        delimeter = "\t") {

  if(!(type %in% c("csv", "excel", "text"))){
    stop("Must provide support file type (\"csv\", \"excel\", or \"excel\"")
  }

  df <- switch(type,
         "csv" = read.csv(filename),
         "excel" = read_excel(filename),
         "text" = read.table(filename, sep = delimeter))

  # cleanData()

  return(df)

}

#' Cleans the data in the data frame
#'
#' @param df a dataframe to be cleaned
#' @param removeNA a boolean to remove rows with NA values
#' @param removeDuplicates a boolean to remove duplicate rows
#' @param cleanNames a boolean to clean the column names using the janitor package
#'
#' @return A cleaned dataframe
cleanData <- function(df, removeNA, removeDuplicates, cleanNames) {

  return(NULL)

}
