test_that("quick_delin", {
  skip("requires interactivity")

  stimuli <- demo_stim("lisa")
  x <- quick_delin(stimuli, 2)
  plot(x, pt.plot = T)
})
