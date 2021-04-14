stimuli <- demo_stim("london", "001_03")

test_that("no map", {
  m <- mirror(stimuli)
  m2 <- mirror(m)
  expect_equal(m[[1]]$points[2,], m2[[1]]$points[2,])
  expect_false(all(m[[1]]$points[1,] == m2[[1]]$points[1,]))
  expect_equal(stimuli, m2)
  # left-eye vs right-eye: > for m, < for m2
  expect_true(m[[1]]$points[1, 1] > m[[1]]$points[1, 2])
  expect_true(m2[[1]]$points[1, 1] < m2[[1]]$points[1,2])

  m <- mirror(stimuli, axis = "horizontal")
  m2 <- mirror(m, axis = "horizontal")
  expect_equal(m[[1]]$points[1,], m2[[1]]$points[1,])
  expect_false(all(m[[1]]$points[2,] == m2[[1]]$points[2,]))
  expect_equal(stimuli, m2)
  # chin vs forehead: < for m, > for m2
  expect_true(m[[1]]$points[2, 130] < m[[1]]$points[2, 140])
  expect_true(m2[[1]]$points[2, 130] > m2[[1]]$points[2, 140])
})

test_that("map", {
  m <- mirror(stimuli, "frl")
  m2 <- mirror(m, "frl")
  expect_false(all(m[[1]]$points[1,] == m2[[1]]$points[1,]))
  expect_false(all(m[[1]]$points[2,] == m2[[1]]$points[2,]))
  expect_equal(stimuli, m2)
  # left-eye < right-eye for both
  expect_true(m[[1]]$points[1, 1] < m[[1]]$points[1, 2])
  expect_true(m2[[1]]$points[1, 1] < m2[[1]]$points[1,2])
})

test_that("horiz", {
  m <- mirror(stimuli, axis = "horizontal")
  expect_true(m[[1]]$points[1, 1] < m[[1]]$points[1, 2])
})

test_that("no tem", {
  notem <- remove_tem(stimuli)
  m_no <- mirror(notem)
  m <- mirror(stimuli)
  expect_equal(m[[1]]$img, m_no[[1]]$img)
})
