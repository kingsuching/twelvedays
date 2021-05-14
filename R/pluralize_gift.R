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
  if(str_detect(gift, "y$")) {
    gift <- str_replace(gift, "y$", "ies")
  }else{
    gift <- str_replace(gift, "$", "s")
  }
  return(gift)

}

sapply(xmas$Gift.Item, pluralize_gift)
