# Creates basis of personnel list
positions <- read_csv("data-raw/positions.csv")
positions <- as_tibble(lapply(positions, rep, positions$N))

usethis::use_data(positions, overwrite = TRUE)
