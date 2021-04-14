path <- system.file("extdata/composite/f_multi.tem", package = "webmorphR")
stimuli <- read_stim(path)

test_that("works", {
  nt <- pt_delete(stimuli, 0:9, 20:188)
  expect_equal(stimuli[[1]]$points[, 11:20], nt[[1]]$points)
})
