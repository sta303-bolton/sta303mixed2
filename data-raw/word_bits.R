## The initials, final and vowels are pulled
## the gibberish database from https://github.com/greghaskins/gibberish

bits <- yaml::read_yaml("https://raw.githubusercontent.com/greghaskins/gibberish/master/gibberish/database/raw_components.yaml")

simpler <- yaml::read_yaml("https://raw.githubusercontent.com/greghaskins/gibberish/master/gibberish/database/components.yaml")

initials <- simpler$initials[[1]] %>%
  as_tibble()

usethis::use_data(initials, overwrite = TRUE)

finals <- bits$finals %>%
  unlist() %>%
  as_tibble() %>%
  filter(!(value %in% c("TRUE", "FALSE"))) %>%
  filter(!str_detect(value, "z|x|ct")) %>%
  filter(str_length(value) <= 2) %>%
  slice(1:27)

usethis::use_data(finals, overwrite = TRUE)

vowels <- bits$vowels %>%
  unlist() %>%
  as_tibble() %>%
  filter(!(value %in% c("TRUE", "FALSE"))) %>%
  filter(str_length(value) <= 2) %>%
  filter(!(value %in% c("ii", "uu")))

usethis::use_data(vowels, overwrite = TRUE)
