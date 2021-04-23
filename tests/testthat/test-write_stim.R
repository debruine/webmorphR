test_that("works", {
  stimuli <- demo_stim()[1]
  dir <- file.path(tempdir(), "ftest")

  # default format ----
  write_stim(stimuli, dir)
  expect_equal(list.files(dir), c("f_multi.png", "f_multi.tem"))
  unlink(dir, recursive = TRUE)
  
  # rename ----
  write_stim(stimuli, dir, "newname")
  expect_equal(list.files(dir), c("newname.png", "newname.tem"))
  unlink(dir, recursive = TRUE)
  
  write_stim(demo_stim(), dir, "newname")
  expect_equal(list.files(dir), c("newname_1.png", "newname_1.tem", "newname_2.png", "newname_2.tem"))
  unlink(dir, recursive = TRUE)
  
  write_stim(demo_stim(), dir, c("A", "B"))
  expect_equal(list.files(dir), c("A.png", "A.tem", "B.png", "B.tem"))
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
