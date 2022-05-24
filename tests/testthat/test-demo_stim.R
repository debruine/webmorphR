test_that("demo_stim", {
  x <- demo_stim()
  expect_equal(names(x), c("f_multi", "m_multi"))
  
  f <- demo_stim("f")
  expect_equal(names(f), c("f_multi"))
  
  tem <- demo_tems()
  expect_equal(names(tem), c("dlib7", "dlib70", "fpp106", "fpp83", "frl"))
  
  dlib <- demo_tems("dlib")
  expect_equal(names(dlib), c("dlib7", "dlib70"))
})
