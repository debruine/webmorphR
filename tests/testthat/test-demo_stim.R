test_that("demo_stim", {
  expect_error(demo_stim("no"))
  x <- demo_stim()
  expect_equal(names(x), c("f_multi", "m_multi"))
  
  f <- demo_stim("test", "f")
  expect_equal(names(f), c("f_multi"))
  
  tem <- demo_stim("tem_examples")
  expect_equal(names(tem), c("dlib7", "dlib70", "fpp106", "fpp83", "frl"))
})
