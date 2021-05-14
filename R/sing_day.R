#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, line, phrase_col){

  phrases <- dataset %>% pull({{phrase_col}})
  map(dataset, paste("On the", line, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6)))
  dataset$English <- as.character(english(dataset$Day))

  dataset <- dataset %>%
    mutate(
      Full.Phrase = pmap_chr(dataset, ~make_phrase(..1, ..7, ..3, ..4, ..5, ..6))
    )
  final <- paste("On the", dataset$Day.In.Words, "day of Christmas my true love gave to me,", dataset$Full.Phrase)
  return(final)

}
xmas$Phrase <- make_phrase(xmas$Day, xmas$English, xmas$Gift.Item, xmas$Verb, xmas$Adjective, xmas$Location)

sing_day(xmas, 2, xmas$Full.Phrase)
