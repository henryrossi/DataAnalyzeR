#' Loads a file given a format, with optional treatment of NA values
#'
#' @param file The name of the file to be imported
#' @param file The type of the file ("csv" = (.csv), "excel" = (.xls, .xlsx) "text" = (txt))
#' @param removeNA a boolean to remove rows with NA values
#' @param removeDuplicates a boolean to remove duplicate rows
#' @param cleanNames a boolean to clean the column names using the janitor package
#'
#' @return A cleaned dataframe
#'
#' @export
cleanImport <- function(file, type,
                        removeNA = TRUE,
                        removeDuplicates = TRUE,
                        cleanNames = TRUE) {

  cleanData()

  return(NULL)

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
