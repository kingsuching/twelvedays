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
#' @import english
#'
#' @export
sing_day <- function(dataset, line){
  phrases <- dataset %>% pull({{phrase_col}})
  dataset$English <- as.character(english(dataset$Day))
  dataset <- filter(dataset, Day <= line)
  constant_date <- filter(dataset, Day == line)$Day.in.Words
  cat(paste("On the", constant_date, "day of Christmas my true love gave to me,", "\n"))
  dataset <- dataset %>%
    mutate(
         Full.Phrase = pmap_chr(dataset, ~make_phrase(..1, ..7, ..3, ..4, ..5, ..6))
    )
  final <- paste(dataset$Full.Phrase) %>%
    str_replace("[ ]+", " ")
  final <- str_trim(final)
  cat(rev(final), sep = "\n")
}
