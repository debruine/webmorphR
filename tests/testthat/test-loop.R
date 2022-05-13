#wm_opts(server = "https://webmorph.test")

test_that("loop", {
  stimuli <- demo_stim()
  loop <- loop(stimuli, 2)
  nm <- c("f_multi_m_multi_1", "f_multi_m_multi_2",
          "m_multi_f_multi_1", "m_multi_f_multi_2")
  expect_equal(names(loop), nm)
  
  revloop <- loop(stimuli[2:1], 2)
  nm <- c("m_multi_f_multi_1", "m_multi_f_multi_2",
          "f_multi_m_multi_1", "f_multi_m_multi_2")
  expect_equal(names(revloop), nm)
  
  ## visual checks ----
  # stimuli <- demo_stim("composite", c(1,6,7,2,4,9,10,5)) |>
  #   resize(300)
  # loop <- loop(stimuli, 5)
  # animate(loop, 10)
})

wm_opts(server = "https://webmorph.org")