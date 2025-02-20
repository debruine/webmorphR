stimuli <- demo_stim()

test_that("error", {
  expect_error(metrics(stimuli, "x[s]"))
  expect_equal(metrics(stimuli, "x[200]"), c(f_multi = NA_real_, m_multi = NA_real_))
})

test_that("single point", {
  x0 <- c(f_multi = stimuli[[1]]$points[["x", 1]],
         m_multi = stimuli[[2]]$points[["x", 1]])
  y0 <- c(f_multi = stimuli[[1]]$points[["y", 1]],
          m_multi = stimuli[[2]]$points[["y", 1]])
  expect_equal(metrics(stimuli, "x[0]"), x0)
  expect_equal(metrics(stimuli, "y[0]"), y0)
})

test_that("eye spacing", {
  es <- metrics(stimuli, c(0, 1))
  comp <- c(f_multi = 77.40749, m_multi = 75.37856)
  
  expect_equal(es, comp, tol = .0001)
})

test_that("formula", {
  eye_spacing <- "sqrt(pow(x[0]-x[1], 2) + pow(y[0]-y[1],2))"
  es <- metrics(stimuli, eye_spacing)
  comp <- c(f_multi = 77.40749, m_multi = 75.37856)
  
  expect_equal(es, comp, tol = .0001)
})
