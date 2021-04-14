path <- system.file("extdata/composite", package = "webmorphR")
stimuli <- read_stim(path, "f_multi")

test_that("error", {
  expect_error(resize(),
               'argument "stimuli" is missing, with no default',
               fixed = TRUE)

  expect_error(resize(list("a")),
               'stimuli needs to be a stimlist',
               fixed = TRUE)

  expect_error(resize(stimuli, -2),
               "width must be a positive number",
               fixed = TRUE)

  expect_error(resize(stimuli, 1, -2),
               "height must be a positive number",
               fixed = TRUE)
})

test_that("basic", {
  no_change <- resize(stimuli)
  expect_equal(no_change, stimuli)

  # %, no height
  r <- resize(stimuli, .5)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimuli[[1]]$width, r[[1]]$width*2)
  expect_equal(stimuli[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimuli[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimuli[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # % no width
  r <- resize(stimuli, height = .5)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimuli[[1]]$width, r[[1]]$width*2)
  expect_equal(stimuli[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimuli[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimuli[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # % different height/width
  r <- resize(stimuli, width = 0.25, height = .50)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(round(stimuli[[1]]$width/4), r[[1]]$width)
  expect_equal(stimuli[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimuli[[1]]$points[1, 1],
               4*r[[1]]$points[1, 1])
  expect_equal(stimuli[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels, no height
  r <- resize(stimuli, 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimuli[[1]]$width, r[[1]]$width*2)
  expect_equal(stimuli[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimuli[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimuli[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels no width
  r <- resize(stimuli, height = 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimuli[[1]]$width, r[[1]]$width*2)
  expect_equal(stimuli[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimuli[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimuli[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels different height/width
  r <- resize(stimuli, width = 1350/4, height = 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(round(1350/4), r[[1]]$width)
  expect_equal(1350/2, r[[1]]$height)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimuli[[1]]$points[1, 1],
               4*r[[1]]$points[1, 1])
  expect_equal(stimuli[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])
})

test_that("no tem", {
  tem <- demo_stim("test")
  notem <- remove_tem(tem)
  x <- resize(notem, .5)
  expect_equal(width(x), width(notem)/2)
  expect_equal(height(x), height(notem)/2)
})
