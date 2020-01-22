## code to prepare `warsaw_queues` dataset goes here
warsaw_queues <- read.csv("data-raw/warsaw_queues.csv")

usethis::use_data(warsaw_queues, internal = FALSE, overwrite = TRUE)
