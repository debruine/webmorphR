test_that("compare", {
  # s <- demo_stim()
  # cont <- continuum(s[1], s[2], by = 0.25)
  # write_stim(cont, "tests/testthat/_data", format = "jpg")
  # setwd("tests/testthat/")
  cont <- read_stim("_data")
  
  # ref is stimlist
  diff <- compare(cont, cont[1])
  expect_equal(diff[[1]], 0, tol = .0001)
  expect_true(diff[1] < diff[2])
  expect_true(diff[2] < diff[3])
  expect_true(diff[3] < diff[4])
  expect_true(diff[4] < diff[5])
  
  # ref is stim
  diff2 <- compare(cont, cont[[2]])
  expect_equal(diff2[[2]], 0, tol = .0001)
  expect_true(diff2[1] > diff2[2])
  expect_true(diff2[2] < diff2[3])
  expect_true(diff2[3] < diff2[4])
  expect_true(diff2[4] < diff2[5])
  
  # ref is character
  diff3 <- compare(cont, "3")
  expect_equal(diff3[[3]], 0, tol = .0001)
  expect_true(diff3[1] > diff3[2])
  expect_true(diff3[2] > diff3[3])
  expect_true(diff3[3] < diff3[4])
  expect_true(diff3[4] < diff3[5])
  
  # ref is numeric
  diff4 <- compare(cont, 4)
  expect_equal(diff4[[4]], 0, tol = .0001)
  expect_true(diff4[1] > diff4[2])
  expect_true(diff4[2] > diff4[3])
  expect_true(diff4[3] > diff4[4])
  expect_true(diff4[4] < diff4[5])
})
