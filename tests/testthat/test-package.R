test_that("loading", {
  default_opts <- list(
    webmorph.verbose = TRUE,
    webmorph.line.color = "blue",
    webmorph.pt.color = "green",
    webmorph.fill = "white",
    webmorph.server = "https://webmorph.org",
    webmorph.plot = "inline",
    webmorph.plot.maxwidth = 10000,
    webmorph.plot.maxheight = 10000
  )
  
  for (i in seq_along(default_opts)) {
    opt_val <- options(names(default_opts[i]))
    expect_equal(opt_val, default_opts[i])
  }
})
