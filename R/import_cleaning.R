#' Loads in a file and cleans it according to user specification, returning the result in a dataframe
#'
#' @param fileName The name of the file to be imported
#' @param sheetName The name of the sheet to be opened (only for xls and xlsx files)
#' @param headerRow a boolean if the file contains a header row (only for txt files)
#' @param removeNA a boolean to remove rows with NA values
#' @param removeDuplicates a boolean to remove duplicate rows
#' @param cleanNames a boolean to clean the column names using the janitor package
#'
#' @importFrom readxl read_excel read_xlsx
#' @importFrom utils read.table read.csv
#' @importFrom stringr str_subset
#'
#' @return A cleaned dataframe
#'
#' @export
cleanImport <- function(fileName,
                        sheetName = 1,
                        headerRow = TRUE,
                        removeNA = TRUE,
                        removeDuplicates = TRUE,
                        cleanNames = TRUE) {

  # get the type of the file from the file extension
  type <- sub(".*\\.([^\\.]*)$", "\\1", fileName)

  # stop and provide an error if the file type is not supported
  if(!(type %in% c("csv", "xls", "xlsx", "txt"))){
    stop("Must provide supported file type (.csv, .txt, .xls, or .xlsx)")
  }

  # depending on the type of the file, load in the data
  df <- switch(type,
         "csv" = read.csv(fileName),
         "txt" = read.table(fileName, header = headerRow),
         "xls" = read_excel(fileName, sheet = sheetName),
         "xlsx" = read_xlsx(fileName, sheet = sheetName))

  # pass in the new dataframe into the data cleaning helper function
  df <- cleanData(df, removeNA, removeDuplicates, cleanNames)

  # return the final cleaned dataframe
  return(as.data.frame(df))

}

#' Cleans the data in the data frame according to user specification
#'
#' @param df a dataframe to be cleaned
#' @param removeNA a boolean to remove rows with NA values
#' @param removeDuplicates a boolean to remove duplicate rows
#' @param cleanNames a boolean to clean the column names using the janitor package
#'
#' @importFrom tidyr drop_na
#' @importFrom dplyr distinct
#' @importFrom janitor clean_names
#'
#' @return A cleaned dataframe
cleanData <- function(df, removeNA, removeDuplicates, cleanNames) {

  # if the user requested for NAs to be removed
  if(removeNA == TRUE){
    df <- df |>
      drop_na()
  }

  # if the user requested duplicates to be removed
  if(removeDuplicates == TRUE){
    df <- df |>
      distinct(.keep_all = TRUE)
  }

  # if the user requested for names to be cleaned
  if(cleanNames == TRUE){
    df <- df |>
      clean_names()
  }

  # returned the cleaned dataframe
  return(df)

}
