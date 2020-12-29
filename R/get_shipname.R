#' get_shipname
#'
#' @param student_id must be 8 to 10 digits long, numeric
#'
#' @return Single character element with the name of the space ship, created as 'SS' for space ship plus a made up word.
#' @export
#'
#' @examples get_shipname(1002345678)
#'
get_shipname <- function(student_id){
  # errors related to student_id
  if(!is.numeric(student_id)) stop("student_id must be numeric, there should be no quote marks or letters. You student number is not your UTORid.")
  if(!(nchar(student_id) %in% c(8, 9, 10))) stop("Are you sure that is your student id? It should be 8 to 10 digits (10 is most common).")

  # load data needed?
  data("initials", envir=environment())
  data("vowels", envir=environment())
  data("finals", envir=environment())

  set.seed(student_id)
  rand <- runif(1, 0, 1)
  ship_name <- dplyr::if_else(rand > 0.5,
          paste0("SS ", stringr::str_to_title(paste0(sample(initials$value, 1),
                                          sample(vowels$value, 1),
                                          sample(finals$value, 1)))),
          paste0("SS ", stringr::str_to_title(paste0(sample(initials$value, 1),
                                          sample(vowels$value, 1),
                                          sample(initials$value, 1),
                                          sample(vowels$value, 1),
                                          sample(finals$value, 1)))))
  assign("ship_name", ship_name, .GlobalEnv)
}

