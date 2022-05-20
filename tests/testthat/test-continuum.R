#wm_opts(server = "https://webmorph.test")

test_that("continuum", {
  skip_on_cran()
  
  stimuli <- demo_stim()
  
  cont <- continuum(stimuli[1], stimuli[2], by = 0.2)
  
  # compare image metrics
  comp <- compare(cont, ref_stim = cont[[1]], scale = TRUE)
  expect_equal(comp[[1]], 0)
  expect_true(comp[[1]] < comp[[2]])
  expect_true(comp[[2]] < comp[[3]])
  expect_true(comp[[3]] < comp[[4]])
  expect_true(comp[[4]] < comp[[5]])
  expect_true(comp[[5]] < comp[[6]])
  expect_equal(comp[[6]], 1)
})

wm_opts(server = "https://webmorph.org")