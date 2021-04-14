test_that("%||%", {
  x <- NULL %||% 2 %||% 1
  expect_equal(x, 2)

})
