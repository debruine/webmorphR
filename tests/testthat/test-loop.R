#wm_opts(server = "https://webmorph.test")

test_that("loop", {
  skip_on_cran()
  skip_if_offline()
  
  stimuli <- demo_stim()
  expect_error(loop(stimuli[1]))
  expect_error(loop(stimuli, 1))
  
  
  loop <- loop(stimuli, 2)
  nm <- c("f_multi_m_multi_1", "f_multi_m_multi_2",
          "m_multi_f_multi_1", "m_multi_f_multi_2")
  expect_equal(names(loop), nm)
  
  revloop <- loop(stimuli[2:1], 2)
  nm <- c("m_multi_f_multi_1", "m_multi_f_multi_2",
          "f_multi_m_multi_1", "f_multi_m_multi_2")
  expect_equal(names(revloop), nm)
  
  # stim with same names
  reploop <- demo_stim() |>
    rep(2) |> 
    rotate(c(0, 30, 60, 90)) |> 
    loop(2)
  
  nm <- c("1_f_multi_2_m_multi_1", "1_f_multi_2_m_multi_2", "2_m_multi_3_f_multi_1", 
          "2_m_multi_3_f_multi_2", "3_f_multi_4_m_multi_1", "3_f_multi_4_m_multi_2", 
          "4_m_multi_1_f_multi_1", "4_m_multi_1_f_multi_2")
  expect_equal(names(reploop), nm)
  
  ## visual checks ----
  # stimuli <- webmorphR.stim::load_stim_composite(c(1,6,7,2,4,9,10,5)) |>
  #   resize(300)
  # loop <- loop(stimuli, 5)
  # animate(loop, 10)
})

wm_opts(server = "https://webmorph.org")