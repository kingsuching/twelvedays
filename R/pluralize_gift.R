#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift){

  if(gift == "goose") {
    return("geese")
  }

  if(str_detect(substr(gift, str_length(gift)-1, str_length(gift)), "y")) {
    gift <- gift %>%
      str_replace("$", "ies")
  }else{
    gift <- gift %>%
      str_replace("$", "s")
  }


  return(gift)

}
