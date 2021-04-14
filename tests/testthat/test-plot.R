test_that("one", {
  x <- demo_stim("test", 1)

  p <- plot(x)

  expect_equal(width(p)[[1]], width(x)[[1]] + 20)
})

test_that("two", {
  x <- demo_stim("test")

  p <- plot(x)
  expect_equal(width(p)[[1]], x[[1]]$width*2+30)
  expect_equal(height(p)[[1]], x[[1]]$height+20)

  p <- plot(x, nrow = 2)
  expect_equal(width(p)[[1]], x[[1]]$width+20)
  expect_equal(height(p)[[1]], x[[1]]$height*2+30)
})

