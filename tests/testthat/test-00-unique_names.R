test_that("basic", {
  full_names <- c("yy.y/xxx/123/A_1.tem", "yy.y/xxx/124/B_1.tem")

  expected <- c("123/A_1", "124/B_1")
  test <- unique_names(full_names)
  expect_equal(unname(test), expected)
  expect_equal(names(test), full_names)

  expected <- c("3/A", "4/B")
  test <- unique_names(full_names, breaks = "")
  expect_equal(unname(test), expected)
  expect_equal(names(test), full_names)

  expected <- c("123/A_1.tem", "124/B_1.tem")
  test <- unique_names(full_names, remove_ext = FALSE)
  expect_equal(unname(test), expected)
  expect_equal(names(test), full_names)
})
