test_that("auto_delin", {
  stimuli <- demo_stim("test", "f")
  expect_error(auto_delin())
  # all images have templates
  expect_warning(x <- auto_delin(stimuli))
  expect_equal(x, stimuli)
  
  expect_error( auto_delin(x, "dd") )
  
  # remove key and secret to test error message
  key <- Sys.getenv("FACEPLUSPLUS_KEY")
  secret <- Sys.getenv("FACEPLUSPLUS_SECRET")
  on.exit(Sys.setenv(FACEPLUSPLUS_KEY = key))
  on.exit(Sys.setenv(FACEPLUSPLUS_SECRET = secret))
  Sys.setenv(FACEPLUSPLUS_KEY = "")
  Sys.setenv(FACEPLUSPLUS_SECRET = "")
  fpp_error <- "You need to set FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET in your .Renviron (see ?auto_delin)"
  expect_error( auto_delin(x, "fpp106", replace = TRUE),
                fpp_error, fixed = TRUE)
  expect_error( auto_delin(x, "fpp83", replace = TRUE),
                fpp_error, fixed = TRUE)
  Sys.setenv(FACEPLUSPLUS_KEY = key)
  Sys.setenv(FACEPLUSPLUS_SECRET = secret)
})

test_that("face++", {
  # Requires FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET
  skip_on_cran()
  skip_if_offline()
  
  stimuli <- demo_stim("test", "f")
  fpp106 <- tem_def("fpp106")
  x106 <- auto_delin(stimuli, "fpp106", replace = TRUE)
  pnames <- (x106$f_multi$points |> dimnames())[[2]]
  expect_equal(pnames, fpp106$points$name)
  expect_equal(x106$f_multi$lines, fpp106$lines)

  fpp83 <- tem_def("fpp83")
  x83 <- auto_delin(stimuli, "fpp83", replace = TRUE)
  pnames <- (x83$f_multi$points |> dimnames())[[2]]
  expect_equal(pnames, fpp83$points$name)
  expect_equal(x83$f_multi$lines, fpp83$lines)
})


test_that("paste 2 together", {
  skip_on_cran()
  skip_if_offline()
  
  s <- demo_stim() |> plot()
  f <- auto_delin(s, "fpp106", TRUE, 1)
  m <- auto_delin(s, "fpp106", TRUE, 2)
  
  expect_true(all((f[[1]]$points == m[[1]]$points) == FALSE))
})

test_that("python", {
  skip_on_cran()
  
  stimuli <- demo_stim("test", "f_")
  
  s7 <- auto_delin(stimuli, "dlib7", TRUE)
  expect_equal(s7[[1]]$points |> dim(), c(2, 7))
  
  #s70 <- auto_delin(stimuli, "dlib70", TRUE)
  #expect_equal(s70[[1]]$points |> dim(), c(2, 70))

  # draw_tem(s2)
  # draw_tem(s3)
})

