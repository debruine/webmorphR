test_that("bounds", {
  stimuli <- demo_stim()
  max1 <- apply(stimuli[[1]]$points, 1, max, simplify = FALSE)
  min1 <- apply(stimuli[[1]]$points, 1, min, simplify = FALSE)
  max2 <- apply(stimuli[[2]]$points, 1, max, simplify = FALSE)
  min2 <- apply(stimuli[[2]]$points, 1, min, simplify = FALSE)
  
  # overall bounds
  b <- bounds(stimuli)
  expect_equal(b$min_x, min(min1$x, min2$x))
  expect_equal(b$max_x, max(max1$x, max2$x))
  expect_equal(b$min_y, min(min1$y, min2$y))
  expect_equal(b$max_y, max(max1$y, max2$y))
  
  # each bounds
  b <- bounds(stimuli, each = TRUE)
  expect_equal(b$min_x[[1]], min1$x)
  expect_equal(b$max_x[[1]], max1$x)
  expect_equal(b$min_y[[1]], min1$y)
  expect_equal(b$max_y[[1]], max1$y)
  expect_equal(b$min_x[[2]], min2$x)
  expect_equal(b$max_x[[2]], max2$x)
  expect_equal(b$min_y[[2]], min2$y)
  expect_equal(b$max_y[[2]], max2$y)
})
