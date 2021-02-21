#' get_my_starship
#'
#' @param student_id must be 8 to 10 digits long, numeric
#'
#' @return Dataset about starship
#' @export
#'
#' @examples get_my_starship(1002345678)
get_my_starship <- function(student_id){
  require(tidyverse)
  # errors related to student_id
  if(!is.numeric(student_id)) stop("student_id must be numeric, there should be no quote marks or letters. You student number is not your UTORid.")
  if(!(nchar(student_id) %in% c(8, 9, 10))) stop("Are you sure that is your student id? It should be 8 to 10 digits (10 is most common).")

     get_shipname(student_id)

     data("positions", envir=environment())

     set.seed(student_id)

     aug_pos <- positions %>%
       mutate(gender = sample(c("Masculine", "Feminine", "Non-binary"), prob = c(0.52, 0.44, 0.04), nrow(.), replace = TRUE)) %>%
       mutate(gen_code = case_when(
         gender == "Masculine" ~ 0,
         gender == "Feminine" ~ 1,
         TRUE ~ 0
       )) %>%
       mutate(eth_code = round(runif(nrow(.), 0.5, 6.49), 0)) %>%
       rowwise() %>%
       mutate(name = randomNames::randomNames(1, gen_code, ethnicity = eth_code,
                                 name.order = "first.last", name.sep = " ")) %>%
       ungroup() %>%
       mutate(duty_shift = sample(c("Alpha", "Beta", "Delta", "Gamma"), nrow(.),
                                  replace = TRUE, prob = c(0.30, 0.30, 0.25, 0.15))) %>%
       mutate(duty_shift = dplyr::if_else(N == 1, "Alpha", duty_shift)) %>%
       group_by(duty_shift, sub_division) %>%
       mutate(shift_team_id = cur_group_id()) %>%
       group_by(duty_shift) %>%
       mutate(shift_team = paste0("Team ", dense_rank(shift_team_id))) %>%
       select(-N, -gen_code, -eth_code) %>%
       ungroup() %>%
       mutate(starfleet_gpa = round(rnorm(nrow(.), 7.6, 1), 2)) %>%
       mutate(starfleet_gpa = case_when(starfleet_gpa > 10 ~ 10,
                                        starfleet_gpa < 4.9 ~ 4.9,
                                        TRUE ~ starfleet_gpa)) %>%
       mutate(person_intercept = runif(nrow(.), 0, 100)) %>%
       mutate(perseverance_score = round(rnorm(nrow(.), 7, 1), 2)) %>%
       mutate(perseverance_score = case_when(perseverance_score > 10 ~ 10,
                                             perseverance_score < 2.9 ~ 2.9,
                                             TRUE ~ perseverance_score)) %>%
       mutate(person_slope = rnorm(nrow(.), 0, 5))

     personnel_data <- as_tibble(lapply(aug_pos, rep, 12)) %>%
       arrange_all() %>%
       mutate(week =  rep(c(1:12), times = nrow(.)/12)) %>%
       rowwise() %>%
       mutate(productivity = week*person_slope + person_intercept +
              10*perseverance_score + 20*starfleet_gpa + -5*shift_team_id + rnorm(1, 0, 10)) %>%
       ungroup() %>%
       mutate(productivity = (productivity - min(productivity))/10+10) %>%
       select(-shift_team_id, -person_intercept, -person_slope) %>%
       group_by(name, position) %>%
       mutate(crew_id = 42000+cur_group_id()) %>%
       ungroup() %>%
       relocate(crew_id)

     assign("crew_data", personnel_data, .GlobalEnv)

}
