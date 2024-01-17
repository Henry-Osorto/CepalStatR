#' list of countries
#'@description
#'A short description...
#'
#' @param language.en
#' If TRUE or omitted is selected, the default language will be English.
#' Select FALSE to choose the Spanish language.
#'
#' @return
#' @export
#'
#' @examples
#' data <- countries()
#'
#'
countries <- function(language.en = TRUE) {

  if(language.en == TRUE){

    data <- call.data(id.indicator = 1, language.en = TRUE)

    country <- data.frame(Countries = unique(data$Country))
  }

  else {
    data <- call.data(id.indicator = 1, language.en = FALSE)

    country <- data.frame(Paises = unique(data$País))

  }


  return(country)
}
