test_that("pad", {
  s <- demo_stim()
  
  # defaults
  x <- pad(s)
  expect_equal(width(s) + 20, width(x))
  expect_equal(height(s) + 20, height(x))
  expect_equal(patch(x[1])[[1]], "#FFFFFFFF")
  
  # proportion
  x <- pad(s, .05)
  expect_equal(width(s) * 1.1, width(x))
  expect_equal(height(s) * 1.1, height(x))
  expect_equal(patch(x[1])[[1]], "#FFFFFFFF")
  
  # pixel
  x <- pad(s, 50)
  expect_equal(width(s) + 100, width(x))
  expect_equal(height(s) + 100, height(x))
  expect_equal(patch(x[2], 100, 100)[[1]], "#FFFFFFFF")
  
  # negative (should I prevent this?)
  x <- pad(s, -50)
  expect_equal(width(s) - 100, width(x))
  expect_equal(height(s) - 100, height(x))
  
  # top = 1, should be 1-pixel border (not delete whole image!)
  x <- pad(s, 1)
  expect_equal(width(s) + 2, width(x))
  expect_equal(height(s) + 2, height(x))
  expect_equal(patch(x[1], 1, 1.1)[[1]], "#FFFFFFFF")
  expect_true(patch(x[1], 100, 100)[[1]] != "#FFFFFFFF")
  
  # fill
  x <- pad(s, fill = "red")
  expect_equal(patch(x[1])[[1]], "#FF0000FF")
  
  x <- pad(s, fill = "#00FF00")
  expect_equal(patch(x[2])[[1]], "#00FF00FF")
  
  x <- pad(s, fill = "rgb(0,0,255)")
  expect_equal(patch(x[1])[[1]], "#0000FFFF")
  
  x <- pad(s, fill = rgb(0, 1, 1))
  expect_equal(patch(x[2])[[1]], "#00FFFFFF")
  
  x <- pad(s, fill = c("red", "green"))
  expect_equal(patch(x)[[1]], "#FF0000FF")
  expect_equal(patch(x)[[2]], "#00FF00FF")
})
