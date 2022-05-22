test_that("auto_delin", {
  stimuli <- demo_stim("test", "f")
  expect_error(auto_delin())
  expect_error( auto_delin(stimuli, "dd") )
  expect_error(x <- auto_delin(stimuli, "dlib7"))
  
  # remove key and secret to test error message
  key <- Sys.getenv("FACEPLUSPLUS_KEY")
  secret <- Sys.getenv("FACEPLUSPLUS_SECRET")
  on.exit(Sys.setenv(FACEPLUSPLUS_KEY = key))
  on.exit(Sys.setenv(FACEPLUSPLUS_SECRET = secret))
  Sys.setenv(FACEPLUSPLUS_KEY = "")
  Sys.setenv(FACEPLUSPLUS_SECRET = "")
  fpp_error <- "You need to set FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET in your .Renviron (see ?auto_delin)"
  expect_error( auto_delin(stimuli, "fpp106", replace = TRUE),
                fpp_error, fixed = TRUE)
  expect_error( auto_delin(stimuli, "fpp83", replace = TRUE),
                fpp_error, fixed = TRUE)
  Sys.setenv(FACEPLUSPLUS_KEY = key)
  Sys.setenv(FACEPLUSPLUS_SECRET = secret)
})

test_that("auto_delin", {
  # Requires FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET
  skip_on_cran()
  skip_if_offline()
  
  # all images have templates
  stimuli <- demo_stim("test", "f")
  expect_warning(x <- auto_delin(stimuli, "fpp106"))
  expect_equal(x, stimuli)
  
  expect_warning( x <- auto_delin(stimuli, "fpp106", 
                                  face = 3, replace = TRUE),
                  "f_multi did not have 3 faces")
  
  stimuli <- demo_stim("test", "m")
  ad <- auto_delin(stimuli, "fpp106", replace = TRUE)
  fpp <- auto_delin(stimuli, "fpp106", replace = TRUE)
  expect_equal(fpp[[1]]$points, ad[[1]]$points)
  expect_equal(fpp[[1]]$lines, ad[[1]]$lines)
})

test_that("paste 2 together", {
  skip_on_cran()
  skip_if_offline()
  
  s <- demo_stim() |> plot()
  f <- auto_delin(s, "fpp106", TRUE, 1)
  m <- auto_delin(s, "fpp106", TRUE, 2)
  
  expect_true(all((f[[1]]$points == m[[1]]$points) == FALSE))
  # draw_tem(c(f, m)) |> plot(nrow = 2)
})


