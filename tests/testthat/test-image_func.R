stimuli <- demo_stim("test")

test_that("errors", {
  expect_error(image_func(stimuli, "xxx"))
  expect_error(image_func(stimuli, 1))
})

test_that("works", {
  testfunc <- function(image) {
    image
  }
  expect_silent(a <- image_func(stimuli, "blur", 5, 3))
  expect_silent(c <- image_func(stimuli, magick::image_blur, 5, 3))
  expect_silent(d <- image_func(stimuli, testfunc))
})

test_that("more", {
  # use magick::image_* functions
  expect_silent(blur <- image_func(stimuli, "blur", 5, 3))
  expect_silent(oilpaint <- image_func(stimuli, "oilpaint", radius = 5))
  expect_silent(negate <- image_func(stimuli, "negate"))
  expect_silent(greenscreen <- image_func(stimuli, "transparent", color = "green", fuzz = 5))
  expect_silent(colorize <- image_func(stimuli, "colorize", opacity = 50, color = "hotpink"))
  expect_silent(sharpen <- image_func(stimuli, "contrast", sharpen = 1))

  # load a logo image and superimpose it on each image
  logo <- system.file("extdata/logo.png", package = "webmorphR") %>%
    magick::image_read() %>%
    magick::image_resize(100)
  expect_silent(badged <- image_func(stimuli, "composite", logo, offset = "+10+10"))
})
