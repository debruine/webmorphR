## code to prepare `london_info` dataset goes here
library(readr)

london_info <- read_csv("https://ndownloader.figshare.com/files/8542042",
                        col_types = cols(
                                      face_id = col_character(),
                                      face_age = col_integer(),
                                      face_sex = col_character(),
                                      face_eth = col_character()
                                    ))[]
london_info$face_id <- paste0(sub("X", "", london_info$face_id), "_03")
london_info$face_eth[[32]] <- "east_asian/white"
names(london_info)[[3]] <- "face_gender"
readr::write_csv(london_info, "data-raw/london_info.csv")

usethis::use_data(london_info, overwrite = TRUE)
