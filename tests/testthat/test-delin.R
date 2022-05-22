test_that("delin", {
  expect_error(delin())
  
  skip_if_not(interactive())
  
  # adjust full delineations
  stimuli <- demo_stim()
  expect_message(x <- delin(stimuli))
  draw_tem(x) |> plot()
  
  # add delineations
  stimuli <- demo_stim() |> remove_tem()
  x2 <- delin(stimuli)
  draw_tem(x2) |> plot()
})
