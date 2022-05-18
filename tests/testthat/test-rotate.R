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

test_that("no img", {
  # image and tem
  img_nokeep <- rotate(stimuli, 45, keep_size = FALSE)
  img_keep <- rotate(stimuli, 45, keep_size = TRUE)
  
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
  
  expect_equal(img_nokeep[[1]]$points, noimg_nokeep[[1]]$points)
  expect_equal(img_keep[[1]]$points, noimg_keep[[1]]$points)
  
  # tem doesn't have w and h, so x and y will be consistently off
  diff = tem_nokeep[[1]]$points - noimg_nokeep[[1]]$points
  udiff <- apply(round(diff, 4), 1, unique)
  expect_equal(length(udiff), 2)
  
  kdiff = tem_keep[[1]]$points - noimg_keep[[1]]$points
  ukdiff <- apply(round(kdiff, 4), 1, unique)
  expect_equal(length(ukdiff), 2)
  
})

# patch ----
test_that("patch", {
  r <- rotate(stimuli, 30, patch = TRUE)
  expect_true(patch(r[[1]]$img) != "#FFFFFFFF")
  
  patch_args <- list(x1 = 1, x2 = 1, y1 = 500, y2 = 500, func = mean)
  p <- rotate(stimuli, 30, patch = patch_args)
  expect_true(patch(r[[1]]$img) != patch(p[[1]]$img))
})


test_that("horiz_eyes", {
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

