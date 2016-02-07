#' @title Read BA Year
#' @description  Scan for the year of BA degree form text file and create a list of ages of
#' Professors.
#' @aliases read BAyear readBA
#' @param dataset Selects the year of the source text file used to create the list
#' @param year Sets the year in which the given data was collected.
#' @param upperadjuster The value of the year manually read from the text for
#'   the upper age bound.  The recommended value for \code{upperadjuster} is 2011 till
#'   2011. Then follow with the year being considered.
#' @param loweradjuster The value of the year manually read from the text for
#'   the lower age bound.  The recommended value for \code{loweradjuster} is 1940.
#' @return A list of ages of professors from the document \code{filename} adjusted with \code{upperadjuster} and
#'   \code{loweradjuster} is given as output.
#' @usage
#' readBAyear(dataset, year, upperadjuster, loweradjuster)

#' @import stringr

#Modifying text file to get the year of BA degree
#' @export
readBAyear <- function(dataset, year, upperadjuster, loweradjuster){
    input <- readLines(system.file("extdata", dataset, package = "facage"), warn = FALSE)
    input <- gsub("[^[:digit:]]","",input)
    input <- str_sub(input, 1, 8)

    #Checking for some strings with other numbers before year of BA and removing them
    for(i in 1:length(input)){
      if(str_sub(input[i], 1, 4) > 2016){
        input[i] <- str_sub(input[i], 3, 6)
      }

      ##Manual adjustment to remove false readings from cases when BA year is not first
      else if(str_sub(input[i], 1, 4) < loweradjuster){
        input[i] <- str_sub(input[i], 5, 8)
      }
      else if(str_sub(input[i], 1, 4) > upperadjuster & str_sub(input[i], 1, 4) < 2016){
        input[i] <- str_sub(input[i], 5, 8)
      }

      #For cases when BA year is first
      else{
        input[i] <- str_sub(input[i], 1, 4)
      }
    }
    input

  #Clean up residual numbers that pass through the above construct
  input <- na.omit(data.frame(BA=as.numeric(input)))
  for(i in 1:dim(input)[1]){
    if(input[i,1] < loweradjuster || input[i,1]==1956 || input[i,1]==1948 || input[i,1]==1955 || input[i,1]==1952 || input[i,1]==1942 || input[i,1]==1946 || input[i,1]==1941){
      input[i,1] <- NA
    }
  }
  input <- na.omit(input)
  output <- data.frame(BA=input$BA, Age=year-input$BA+22)
  output
}
