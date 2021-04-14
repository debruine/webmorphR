path <- system.file("extdata/composite", package = "webmorphR")
stimuli <- read_stim(path, "f_multi")

test_that("works", {
  # no offsets
  ctems <- crop(stimuli, 500, 600)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 500)
  expect_equal(ctems[[1]]$height, 600)
  expect_equal(info$width, 500)
  expect_equal(info$height, 600)
  expect_equal(stimuli[[1]]$points - c(425, 375), ctems[[1]]$points)


  # squash
  ctems <- crop(stimuli, 500, 600, squash = TRUE)
  #plot(ctems, img.plot = TRUE, pt.shape="index")
  expect_equal(ctems[[1]]$points[2,140], c(y = 0))
  expect_equal(ctems[[1]]$points[1,118], c(x = 0))
  expect_equal(ctems[[1]]$points["x",123], c(x = 500-1))
  expect_equal(ctems[[1]]$points["y",146], c(y = 600-1))

  # with offsets
  ctems <- crop(stimuli, 100, 200, 300, 400)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 100)
  expect_equal(ctems[[1]]$height, 200)
  expect_equal(info$width, 100)
  expect_equal(info$height, 200)
  orig_pt <- stimuli[[1]]$points[, 1]
  new_pt <- ctems[[1]]$points[, 1]
  expect_equal(orig_pt, new_pt + c(300, 400))

  # percents, no height
  ctems <- crop(stimuli, .5)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 675)
  expect_equal(ctems[[1]]$height, 1350)
  expect_equal(info$width, 675)
  expect_equal(info$height, 1350)
  expect_equal(stimuli[[1]]$points - c(337.5, 0), ctems[[1]]$points)
})

test_that("different crops", {
  stimuli <- demo_stim("composite")

  w <- seq(.1, 1, .1)
  ctems <- crop(stimuli, w, w)
  expect_equivalent(width(ctems) %>% unname(), 1350*w)
  expect_equivalent(height(ctems) %>% unname(), 1350*w)
})

test_that("no tem", {
  tem <- demo_stim("test")
  notem <- remove_tem(tem)
  x <- crop(notem, 500, 500)
  comp <- c(f_multi = 500, m_multi = 500)
  expect_equal(width(x), comp)
  expect_equal(height(x), comp)
})
