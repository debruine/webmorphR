test_that("image_comp", {
  skip_if_offline()
  skip_on_cran()

  s <- demo_stim()
  cont <- continuum(s[1], s[2], by = 0.25)
  diff <- image_comp(cont, cont[1])
  expect_equal(diff[1], 0)
  expect_true(diff[1] < diff[2])
  expect_true(diff[2] < diff[3])
  expect_true(diff[3] < diff[4])
  expect_true(diff[4] < diff[5])
})
