test_that("quick_delin", {
  skip("requires shiny interaction")

  stimuli <- demo_stim() |> remove_tem()
  x <- quick_delin(stimuli, 2)
  plot(x, pt.plot = T)
})
