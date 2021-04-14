test_that("works", {
  path <- system.file("extdata/composite", package = "webmorphR")
  stimuli <- read_stim(path, "f_multi")
  dir <- paste(tempdir(), "ftest", sep = "/")

  # default format ----
  write_stim(stimuli, dir)
  expect_equal(list.files(dir), c("f_multi.png", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  # formats ----
  write_stim(stimuli, dir, format = "PNG")
  expect_equal(list.files(dir), c("f_multi.png", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  write_stim(stimuli, dir, format = "jpg")
  expect_equal(list.files(dir), c("f_multi.jpg", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  write_stim(stimuli, dir, format = "jpeg")
  expect_equal(list.files(dir), c("f_multi.jpg", "f_multi.tem"))
  unlink(dir, recursive = TRUE)

  write_stim(stimuli, dir, format = "GIF")
  expect_equal(list.files(dir), c("f_multi.gif", "f_multi.tem"))
  unlink(dir, recursive = TRUE)
})
