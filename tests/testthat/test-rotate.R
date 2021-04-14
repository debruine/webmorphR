path <- system.file("extdata/test", package = "webmorphR")
stimuli <- demo_stim()

test_that("works", {
  # 45 degrees with image, keep size
  rtems <- rotate(stimuli, 45)
  expect_equal(rtems[[1]]$width, stimuli[[1]]$width)
  expect_equal(rtems[[1]]$height, stimuli[[1]]$height)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 161.4870, y = 138.6828),
               tolerance = 0.001)

  # 45 degrees with image
  rtems <- rotate(stimuli, 45, keep_size = FALSE)
  expect_equal(rtems[[1]]$width, 480)
  expect_equal(rtems[[1]]$height, 480)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 232.4870, y = 209.6828),
               tolerance = 0.001)


  # no images, so estimates centre of image
  stimuli <- read_stim(path, "tem$")
  rtems <- rotate(stimuli, 45, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 479)
  expect_equal(rtems[[1]]$height, 479)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 233.2321, y = 208.6849),
               tolerance = 0.001)

  # negative rotation, no images
  rtems <- rotate(stimuli, -45, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 479)
  expect_equal(rtems[[1]]$height, 479)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 208.6849, y = 246.2205),
               tolerance = 0.001)

  # > 360 rotation, images
  stimuli <- demo_stim()
  rtems <- rotate(stimuli, 360+90, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 338)
  expect_equal(rtems[[1]]$height, 338)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 185.125, y = 142.250),
               tolerance = 0.001)
})

test_that("no tem", {
  tem <- demo_stim("lisa")
  notem <- remove_tem(tem)
  x <- rotate(notem, 90, keep_size = FALSE)
  expect_equal(width(x), height(notem))
  expect_equal(height(x), width(notem))
})


test_that("horiz_eyes", {
  stimuli <- demo_stim("lisa")

  z <- horiz_eyes(stimuli, left_eye = 0, right_eye = 1)
  pt <- lapply(z, `[[`, "points") %>%
    sapply(`[`, 2, 1:2)

  expect_equal(pt[1, ], pt[2, ], tolerance = .0001)
})
