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
#' @import english
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location){

  verb <- str_replace_na(verb, "")
  num_word_temp <- num_word
  if(num > 1) {
    num_word_temp <- num_word
    item_temp <- map_chr(item, pluralize_gift)
    if(num <= 5) {
      return(paste(num_word_temp, adjective, item_temp))
    }
  }else{
    num_word_temp <- "and a"
    item_temp <- item
  }
  phrase <- paste(num_word_temp, adjective, item_temp, verb, location, sep = " ") %>%
    str_remove("NA") %>%
    str_remove(" NA") %>%
    str_trim()
  return(phrase)
}
xmas$English <- as.character(english(xmas$Day))

xmas <- xmas %>%
  mutate(
    Full.Phrase = pmap_chr(xmas, ~make_phrase(..1, ..7, ..3, ..4, ..5, ..6))
  )
xmas2$English <- as.character(english(xmas2$Day))
pmap_chr(xmas, ~make_phrase(..1, ..7, ..3, ..4, ..5, ..6))
