test_that("width", {
  stimuli <- c(demo_stim("lisa")[1], demo_stim("composite")[1])

  expect_equal(width(stimuli), c(lisa1 = 600, f_african = 1350))
  expect_equal(width(stimuli, "all"), c(lisa1 = 600, f_african = 1350))
  expect_equal(width(stimuli, "min"), 600)
  expect_equal(width(stimuli, "max"), 1350)
  expect_equal(width(stimuli, "unique"), c(600, 1350))
})

test_that("height", {
  stimuli <- c(demo_stim("lisa")[1], demo_stim("composite")[1])

  expect_equal(height(stimuli), c(lisa1 = 800, f_african = 1350))
  expect_equal(height(stimuli, "all"), c(lisa1 = 800, f_african = 1350))
  expect_equal(height(stimuli, "min"), 800)
  expect_equal(height(stimuli, "max"), 1350)
  expect_equal(height(stimuli, "unique"), c(800, 1350))
})

test_that("same_tems", {
  lisa <- demo_stim("lisa")
  test <- demo_stim("test")

  expect_equal(same_tems(lisa), TRUE)
  expect_equal(same_tems(stimuli = test), TRUE)
  expect_equal(c(lisa, test) %>% same_tems(), FALSE)
})

test_that("remove_tem", {
  test <- demo_stim("test")

  expect_equal(names(test$f_multi), c("img", "imgpath", "width", "height",
                                      "tempath", "points", "lines", "closed"))

  test2 <- remove_tem(test)
  expect_equal(names(test2$f_multi), c("img", "imgpath", "width", "height"))
})
