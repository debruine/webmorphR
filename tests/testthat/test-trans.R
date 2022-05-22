#wm_opts(server = "https://webmorph.test")

test_that("trans", {
  skip_on_cran()
  skip_if_offline()
  
  s <- demo_stim()
  
  masc <- trans(s, s$f_multi, s$m_multi, shape = 1)
  expect_equal(names(masc), c("f_multi", "m_multi"))
  
  fem_masc <- trans(s, s$f_multi, s$m_multi,
                    shape = c(fem = -0.5, masc = 0.5))
  
  expect_equal(names(fem_masc), c("f_multi_fem", "m_multi_fem", "f_multi_masc", "m_multi_masc"))
  
  steps <- seq(0, 1, .2)
  faces <- demo_stim() |> resize(0.5)
  cont <- trans(faces$f_multi, faces$f_multi, faces$m_multi, steps, steps, steps)
  
  # compare image metrics
  comp <- compare(cont, ref_stim = 1, scale = TRUE)
  expect_equal(comp[[1]], 0)
  expect_true(comp[[1]] < comp[[2]])
  expect_true(comp[[2]] < comp[[3]])
  expect_true(comp[[3]] < comp[[4]])
  expect_true(comp[[4]] < comp[[5]])
  expect_true(comp[[5]] < comp[[6]])
  expect_equal(comp[[6]], 1)
  
  ## visual checks ----
  # skip("needs visual check")
  # draw_tem(masc) |> label() |> plot()
  # draw_tem(fem_masc) |> label() |> plot(nrow = 2)
  # plot(cont)
  # 
  # # texture
  # s <- demo_stim()
  # tex <- trans(s[1], s[1], s[2], .5, .5, .5)
  # notex <- trans(s[1], s[1], s[2], .5, .5, 0)
})

wm_opts(server = "https://webmorph.org")