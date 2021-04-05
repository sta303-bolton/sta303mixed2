#' get_my_songs
#'
#' @param student_id must be 8 to 10 digits long, numeric
#'
#' @return Dataset of K-pop and Jazz songs
#' @export
#'
#' @examples get_my_songs(1002345678)
get_my_songs <- function(student_id){
  require(tidyverse)
  # errors related to student_id
  if(!is.numeric(student_id)) stop("student_id must be numeric, there should be no quote marks or letters. You student number is not your UTORid.")
  if(!(nchar(student_id) %in% c(8, 9, 10))) stop("Are you sure that is your student id? It should be 8 to 10 digits (10 is most common).")

     data("combo", envir=environment())
     data("combo_genre", envir=environment())
     data("combo_names", envir=environment())

     set.seed(student_id)

     bts <- combo_names %>%
       filter(artist_name == "BTS")

     training_data <- combo_names %>%
       filter(artist_name != "BTS") %>%
       group_by(genre) %>%
       dplyr::sample_n(size = 10) %>%
       bind_rows(bts) %>%
       #mutate(is_kpop = if_else(genre == "k-pop", 1, 0)) %>%
       left_join(combo, by = c("artist_name")) %>%
       ungroup() %>%
       filter(complete.cases(.)) %>%
       rename(id = rowid) %>%
       relocate(id, .before = 1) %>%
       rename(artist_popularity = popularity, artist_followers = followers.total) %>%
       select(id, genre, artist_name, artist_popularity, artist_followers,
              album_name, album_release_date, album_release_year, everything()) %>%
       mutate(artist_name = factor(artist_name))

     testing_data <- combo_names %>%
       mutate(is_kpop = if_else(genre == "k-pop", 1, 0)) %>%
       left_join(combo, by = c("artist_name")) %>%
       ungroup() %>%
       filter(complete.cases(.)) %>%
       rename(id = rowid) %>%
       relocate(id, .before = 1) %>%
       filter(!(id %in% training_data$id)) %>%
       group_by(genre) %>%
       dplyr::sample_n(size = 50) %>%
       rename(artist_popularity = popularity, artist_followers = followers.total) %>%
       select(id, genre, artist_name, artist_popularity, artist_followers,
              album_name, album_release_date, album_release_year, everything()) %>%
       mutate(likely_live = if_else(liveness > 0.8, 1, 0))

     assign("training_data", training_data, .GlobalEnv)
     assign("testing_data", testing_data, .GlobalEnv)

}
