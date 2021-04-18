s <- demo_stim()
s2 <- resize(s, 2)
stimuli <- c(s[[1]], s2[[2]])

test_that("width", {
  expect_equal(width(stimuli), c(f_multi = 500, m_multi = 1000))
  expect_equal(width(stimuli, "all"), c(f_multi = 500, m_multi = 1000))
  expect_equal(width(stimuli, "min"), 500)
  expect_equal(width(stimuli, "max"), 1000)
  expect_equal(width(stimuli, "unique"), c(500, 1000))
})

test_that("height", {
  expect_equal(height(stimuli), c(f_multi = 500, m_multi = 1000))
  expect_equal(height(stimuli, "all"), c(f_multi = 500, m_multi = 1000))
  expect_equal(height(stimuli, "min"), 500)
  expect_equal(height(stimuli, "max"), 1000)
  expect_equal(height(stimuli, "unique"), c(500, 1000))
})

test_that("same_tems", {
  t1 <- demo_stim()
  t2 <- subset_tem(t1, features("gmm"))

  expect_equal(same_tems(t1), TRUE)
  expect_equal(same_tems(stimuli = t2), TRUE)
  expect_equal(c(t1, t2) %>% same_tems(), FALSE)
})

test_that("remove_tem", {
  test <- demo_stim()

  expect_equal(names(test$f_multi), c("img", "imgpath", "width", "height",
                                      "tempath", "points", "lines", "closed"))

  test2 <- remove_tem(test)
  expect_equal(names(test2$f_multi), c("img", "imgpath", "width", "height"))
})
