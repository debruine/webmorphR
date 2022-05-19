test_that("works", {
  stimuli <- demo_stim()
  
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

  # > 360 rotation, images
  rtems <- rotate(stimuli, 360+90, keep_size = FALSE)

  expect_equal(rtems[[1]]$width, 500)
  expect_equal(rtems[[1]]$height, 500)
  expect_equal(rtems[[1]]$points[, 1],
               c(x = 273.5185, y = 210.7407),
               tolerance = 0.001)
})

test_that("no tem", {
  notem <- demo_stim() |> crop(c(0.7, 1.2)) |> remove_tem()
  x <- rotate(notem, 90, keep_size = FALSE)
  expect_equal(width(x), height(notem))
  expect_equal(height(x), width(notem))
})

# no img ----
test_that("no img", {
  stimuli <- demo_stim()
  
  # image and tem
  img_nokeep <- rotate(stimuli, 45, keep_size = FALSE)
  img_keep <- rotate(stimuli, 45, keep_size = TRUE)
  expect_equal(width(stimuli), width(img_keep))
  expect_equal(height(stimuli), height(img_keep))
  expect_true(all(width(stimuli) < width(img_nokeep)))
  expect_true(all(height(stimuli) < height(img_nokeep)))
  
  # no image, but has width and height
  noimg <- stimuli
  noimg[[1]]$img <- NULL
  noimg[[2]]$img <- NULL
  noimg_nokeep <- rotate(noimg, 45, keep_size = FALSE)
  noimg_keep <- rotate(noimg, 45, keep_size = TRUE)
  
  # only tem, no width and height
  tem <- demo_stim("test", ".tem")
  tem_nokeep <- rotate(tem, 45, keep_size = FALSE)
  tem_keep <- rotate(tem, 45, keep_size = TRUE)
  
  # should be same if w/h available
  expect_equal(img_keep[[1]]$points, noimg_keep[[1]]$points)
  
  # magick rotate and mathematical calc of rotation dim are not identical
  diff = img_nokeep[[1]]$points - noimg_nokeep[[1]]$points
  udiff <- apply(round(diff, 4), 1, unique)
  
  # tem doesn't have w and h, so x and y will be consistently off
  diff = tem_nokeep[[1]]$points - noimg_nokeep[[1]]$points
  udiff <- apply(round(diff, 4), 1, unique)
  expect_equal(length(udiff), 2)
  
  kdiff = tem_keep[[1]]$points - noimg_keep[[1]]$points
  ukdiff <- apply(round(kdiff, 4), 1, unique)
  expect_equal(length(ukdiff), 2)
  
})

# reversible ----
test_that("reversible", {
  stimuli <- demo_stim()[1]
  
  for (d in seq(0, 90, 10)) {
    plus30 <- rotate(stimuli, d)
    minus30 <- rotate(plus30, -d)
    
    expect_equal(stimuli$f_multi$points, minus30$f_multi$points)
    expect_equal(width(stimuli), width(minus30))
    expect_equal(height(stimuli), height(minus30))
  }
})

test_that("horiz_eyes", {
  stimuli <- demo_stim()
  z <- stimuli |> rotate(45) |> horiz_eyes()
  pt <- lapply(z, `[[`, "points") |>
    sapply(`[`, 2, 1:2)

  expect_equal(pt[1, ], pt[2, ], tolerance = .0001)
})

# rotated_size ----
test_that("rotated_size", {
  expected <- list(width = sqrt(2*100^2), 
                   height = sqrt(2*100^2))
  
  size <- rotated_size(100, 100, 45)
  expect_equal(size, expected)
  
  size <- rotated_size(100, 100, -45)
  expect_equal(size, expected)
  
  size <- rotated_size(100, 100, 360+45)
  expect_equal(size, expected)
})

