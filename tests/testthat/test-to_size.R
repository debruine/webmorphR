test_that("basic", {
  stimuli <- c(
    demo_stim("test")[[1]],
    demo_stim("lisa")[[1]],
    demo_stim("composite")[[1]],
    demo_stim("zoom")[[1]]
  )

  new1 <- to_size(stimuli, 300, 400, fill = "red")
  expect_equal(width(new1) %>% unname(), rep(300, 4))
  expect_equal(height(new1) %>% unname(), rep(400, 4))

  new2 <- to_size(stimuli, 300, 400, fill = "red", keep_rels = TRUE)
  expect_equal(width(new1) %>% unname(), rep(300, 4))
  expect_equal(height(new1) %>% unname(), rep(400, 4))

  skip("needs visual check")
  plot(new1, pt.plot = TRUE)
  plot(new2, pt.plot = TRUE)
})

test_that("width and height", {
  stimuli <- demo_stim("test")
  expect_error(to_size(stimuli, 10))
  expect_error(to_size(stimuli, 10, 0))
  expect_error(to_size(stimuli, "A", 10))

  x <- to_size(stimuli, c(100, 200), c(300, 400))
  expect_equal(width(x), c(f_multi = 100, m_multi = 200))
  expect_equal(height(x), c(f_multi = 300, m_multi = 400))

  x <- to_size(stimuli, c(100, 200))
  expect_equal(width(x), c(f_multi = 100, m_multi = 100))
  expect_equal(height(x), c(f_multi = 200, m_multi = 200))

  x <- to_size(stimuli, c(h = 200, w = 100))
  expect_equal(width(x), c(f_multi = 100, m_multi = 100))
  expect_equal(height(x), c(f_multi = 200, m_multi = 200))

  x <- to_size(stimuli, c(height = 200, width = 100))
  expect_equal(width(x), c(f_multi = 100, m_multi = 100))
  expect_equal(height(x), c(f_multi = 200, m_multi = 200))
})

test_that("no tem", {
  tem <- demo_stim("test")
  notem <- remove_tem(tem)
  x <- to_size(notem, 100, 200)
  expect_equal(width(x) %>% unname(), c(100, 100))
  expect_equal(height(x) %>% unname(), c(200, 200))
})
