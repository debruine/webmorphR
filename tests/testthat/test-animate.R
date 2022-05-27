test_that("animate", {
  stimuli <- demo_stim()
  x <- animate(stimuli)
  expect_equal(length(x), 1)
  
  img <- x[[1]]$img
  expect_equal(length(img), 2)
  
  info <- magick::image_info(img)
  expect_equal(info$format, rep("gif", 2))
  
  ## args
  x <- demo_stim(1) |> rep(4) |>
    rotate(c(0, 90, 180, 270)) |>
    animate(fps = 4, loop = 2, rev = TRUE)
  
  # rev = TRUE doubles the frames
  img <- x[[1]]$img
  expect_equal(length(img), 8)
  
  # with the second centre having matte == FALSE
  info <- magick::image_info(img)
  expect_equal(info$format, rep("gif", 8))
  expect_equal(info$matte, c(F, F, F, F, T, F, F, F))
})
