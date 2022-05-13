stimuli <- demo_stim()

test_that("works", {
  # 45 degrees with image, keep size
  rtems <- rotate(stimuli, 45)
  expect_equal(rtems[[1]]$width, stimuli[[1]]$width)
  expect_equal(rtems[[1]]$height, stimuli[[1]]$height)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 238.8696, y = 205.6094),
               tolerance = 0.001)

  # 45 degrees with image
  rtems <- rotate(stimuli, 45, keep_size = FALSE)
  expect_equal(rtems[[1]]$width, 710)
  expect_equal(rtems[[1]]$height, 710)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 343.8696, y = 310.6094),
               tolerance = 0.001)


  # no images, so estimates centre of image
  noimg <- stimuli[1]
  noimg[[1]]$img <- NULL
  rtems <- rotate(stimuli, 45, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 710)
  expect_equal(rtems[[1]]$height, 710)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 343.8696, y = 310.6094),
               tolerance = 0.001)

  # negative rotation, no images
  rtems <- rotate(noimg, -45, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 710)
  expect_equal(rtems[[1]]$height, 710)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 310.6094, y = 366.1304),
               tolerance = 0.001)

  # > 360 rotation, images
  rtems <- rotate(stimuli, 360+90, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 500)
  expect_equal(rtems[[1]]$height, 500)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 273.5185, y = 210.7407),
               tolerance = 0.001)
})

test_that("no tem", {
  notem <- stimuli |> crop(c(0.7, 1.2)) |> remove_tem()
  x <- rotate(notem, 90, keep_size = FALSE)
  expect_equal(width(x), height(notem))
  expect_equal(height(x), width(notem))
})


test_that("horiz_eyes", {
  z <- stimuli |> rotate(45) |> horiz_eyes()
  pt <- lapply(z, `[[`, "points") |>
    sapply(`[`, 2, 1:2)

  expect_equal(pt[1, ], pt[2, ], tolerance = .0001)
})
