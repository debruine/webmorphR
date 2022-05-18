test_that("quick_delin", {
  skip_if_not(interactive())

  stimuli <- demo_stim() |> remove_tem()
  x <- quick_delin(stimuli, 2)
  plot(x, pt.plot = T)
})
