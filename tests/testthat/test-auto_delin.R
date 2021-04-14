test_that("auto_delin", {
  stimuli <- demo_stim("test", "f")
  expect_error(auto_delin())
  # all images have templates
  expect_warning(x <- auto_delin(stimuli))
  expect_equal(x, stimuli)

  # Requires FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET
  skip_on_cran()
  skip_if_offline()
  fpp106 <- tem_def("fpp106")
  x106 <- auto_delin(stimuli, replace = TRUE)
  pnames <- (x106$f_multi$points %>% dimnames())[[2]]
  expect_equal(pnames, fpp106$points$name)
  expect_equal(x106$f_multi$lines, fpp106$lines)

  fpp83 <- tem_def("fpp83")
  x83 <- auto_delin(stimuli, "fpp83", replace = TRUE)
  pnames <- (x83$f_multi$points %>% dimnames())[[2]]
  expect_equal(pnames, fpp83$points$name)
  expect_equal(x83$f_multi$lines, fpp83$lines)
})
