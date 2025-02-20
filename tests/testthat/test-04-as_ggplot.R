test_that("as_ggplot", {
  stimuli <- demo_stim()
  p <- plot(stimuli)
  
  # from plot
  gg <- as_ggplot(p)
  expect_equal(class(gg), c("gg", "ggplot"))
  
  # from stimuli
  gg2 <- as_ggplot(stimuli)
  expect_equal(class(gg2), c("gg", "ggplot"))
  expect_equal(gg2$layers[[2]]$geom_params$xmax, 1000 + 30)
  expect_equal(gg2$layers[[2]]$geom_params$ymin, 500 + 20)
  
  # pass arguments to plot
  gg3 <- as_ggplot(stimuli, padding = 0)
  expect_equal(class(gg3), c("gg", "ggplot"))
  expect_equal(gg3$layers[[2]]$geom_params$xmax, 1000)
  expect_equal(gg3$layers[[2]]$geom_params$ymin, 500)
})
