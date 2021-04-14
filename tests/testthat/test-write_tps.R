stimuli <- demo_stim("composite")

test_that("works", {
  tpath <- tempfile(fileext = ".tps")
  tps <- write_tps(stimuli, tpath)
  sink <- capture.output(g_array <- geomorph::readland.tps(
    tpath, specID = "ID", warnmsg = FALSE
  ))

  w_array <- tems_to_array(stimuli)

  expect_equivalent(dim(g_array), dim(w_array))
  expect_equal(dim(w_array), c(189, 2, 10))

})
