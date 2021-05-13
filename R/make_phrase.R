#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location){

  verb <- str_replace_na(verb, "")

if(num == 1) {
  num_word = "and a"
}else{
  item <- map_chr(item, pluralize_gift)
}
if(num <= 5) {
  paste(num_word, verb, adjective, item, location) %>%
    str_trim() %>%
    str_remove("NA") %>%
    str_remove(" NA") %>%
    str_trim()
}
  return(paste(num_word, item, verb, adjective, location) %>%
    str_trim() %>%
    str_remove("NA") %>%
    str_remove(" NA") %>%
    str_trim())


}
pmap(list(xmas$Day, xmas$Day.in.Words, xmas$Gift.Item, xmas$Verb, xmas$Adjective, xmas$Location), make_phrase)
