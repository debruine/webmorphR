# basic ----
test_that("get_point basic", {
  stimuli <- demo_stim()
  pt <- get_point(stimuli)
  
  expect_equal(names(pt), c("image", "point", "x", "y"))
  expect_equal(pt$x[[1]], stimuli[[1]]$points[['x', 1]])
  expect_equal(pt$y[[1]], stimuli[[1]]$points[['y', 1]])
  expect_equal(pt$x[[2]], stimuli[[2]]$points[['x', 1]])
  expect_equal(pt$y[[2]], stimuli[[2]]$points[['y', 1]])
  expect_equal(pt$image, names(stimuli))
  expect_equal(pt$point, c(0, 0))
})

# select points ----
test_that("get_point select points", {
  stimuli <- demo_stim()
  pt <- get_point(stimuli, pt = 11:15)
  
  expect_equal(names(pt), c("image", "point", "x", "y"))
  expect_equal(10, nrow(pt))
  expect_equal(stimuli[[1]]$points['x', (11:15)+1], pt$x[1:5])
  expect_equal(stimuli[[1]]$points['y', (11:15)+1], pt$y[1:5])
})
