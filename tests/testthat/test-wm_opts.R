test_that("opts", {
  opts <- wm_opts()
  expected <- list(
    connection = stdin(),
    fill = "white",
    line.color = "blue",
    overwrite = "ask",
    plot = "inline",
    plot.maxheight = 10000,
    plot.maxwidth = 10000,
    pt.color = "green",
    server = "https://webmorph.org",
    verbose = TRUE
  )
  
  defaults <- wm_opts_defaults()
  
  expect_equal(opts, expected)
  expect_equal(opts, defaults)
  
  expect_equal(wm_opts("fill"), expected$fill)
  expect_equal(wm_opts("verbose"), expected$verbose)
  
  # set options
  wm_opts("fill" = "black")
  wm_opts("verbose" = FALSE)
  
  expect_equal(wm_opts("fill"), "black")
  expect_equal(wm_opts("verbose"), FALSE)
  
  # set with list
  wm_opts(defaults)
  expect_equal(opts, expected)
})
