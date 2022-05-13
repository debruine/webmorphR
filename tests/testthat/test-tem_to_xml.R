test_that("tem_to_xml", {
  skip_on_cran() # requires a python installation with dlib
  
  stimuli <- demo_stim()
  dir <- tempfile()
  xml <- tem_to_xml(stimuli, dir, "Demo Stim")
  xml10 <- readLines(xml, n = 10)
  
  expect_equal(xml10[[1]], "<?xml version='1.0' encoding='ISO-8859-1'?>")
  expect_equal(xml10[[4]], "<name>Demo Stim</name>")
  expect_equal(xml10[[6]], paste0("  <image file='", normalizePath(dir), "/f_multi.jpg'>"))
  expect_equal(xml10[[7]],"    <box top='180' left='159' width='186' height='186'>")
})
